#!/usr/bin/env python3

# import packages
import sys
import argparse
import io
import pandas as pd
import numpy as np
from ete3 import Tree


sys.setrecursionlimit(100000)

# helper variables
STOP_UNKNOWN = 0
STOP_DIVERSIFICATION = 1
STOP_EXTINCTION_WOS = 2
STOP_SAMPLING = 3
STOP_TIME = 4

HISTORY = 'history'

STOP_REASON = 'stop_reason'
SAMPLING = 'sampling'
DIVERSIFICATION = 'diversification'
DIST_TO_START = 'DIST_TO_START'
PROCESSED = 'processed'


def simulate_bd_tree_gillespie(birth_r, extinction_r, sampling_f, max_s, max_t):
    """
    Simulates the tree evolution from a root over the given time
    based on the given diversification rate, extinction rate and sampling probability
    :param birth_r: float of diversification rate
    :param extinction_r: float of extinction rate
    :param sampling_f: float, between 0 and 1, fraction of species to be sampled
    :param max_s: maximum number of sampled leaves in a tree
    :param max_t: maximum time from root simulation
    :return: the simulated tree (ete3.Tree).
    """
    # init checker of right size of the tree
    right_size = 0
    # init trial counter (due to stochastic extinction of the tree)
    trial = 0

    while right_size == 0 and trial < 100:
        root = Tree(dist=0)
        root.add_feature(DIST_TO_START, 0)

        # evolve it till we get the correct number of tips - Gillespie
        time = 0
        number_living_strains = 1
        living_strains = root.get_leaves()
        number_sampled = 0

        total_branches = 1
        total_removed = 0
        max_unsampled_tips = max_s/sampling_f

        while 0 < number_living_strains < max_unsampled_tips and time < max_t:
            # re-calculate the rates and their sum
            birth_rate_i = birth_r * number_living_strains
            extinction_rate_i = extinction_r * number_living_strains
            sum_rates = birth_rate_i + extinction_rate_i

            # time of next event
            time_to_next = np.random.exponential(1 / sum_rates, 1)[0]
            time = time + time_to_next

            # leaf affected by next event
            nb_which_leaf = int(np.floor(np.random.uniform(0, len(living_strains), 1)))
            which_leaf = living_strains[nb_which_leaf]
            del living_strains[nb_which_leaf]

            # now let us see which event will happen
            random_event = np.random.uniform(0, 1, 1) * sum_rates
            which_leaf.dist = abs(time - which_leaf.DIST_TO_START)
            which_leaf.add_feature(DIST_TO_START, time)

            if random_event < birth_rate_i:
                total_branches += 1
                # there will be a diversification event
                number_living_strains += 1
                which_leaf.add_feature(STOP_REASON, STOP_DIVERSIFICATION)

                recipient, donor = which_leaf.add_child(dist=0), which_leaf.add_child(dist=0)
                donor.add_feature(DIST_TO_START, which_leaf.DIST_TO_START)
                recipient.add_feature(DIST_TO_START, which_leaf.DIST_TO_START)

                # add original and new lineage on the list
                living_strains.append(donor)
                living_strains.append(recipient)

            else:
                # there will be an extinction event
                number_living_strains -= 1
                total_removed += 1
                del which_leaf

        # we sample x leaves at the end of simulation
        nb_leaves_to_sample = max_s

        if number_living_strains > max_s and time < max_t:
            while number_sampled < nb_leaves_to_sample:
                nb_which_leaf = int(np.floor(np.random.uniform(0, len(living_strains), 1)))
                which_leaf = living_strains[nb_which_leaf]
                del living_strains[nb_which_leaf]

                which_leaf.dist = abs(time - which_leaf.DIST_TO_START)
                which_leaf.add_feature(DIST_TO_START, time)
                which_leaf.add_feature(STOP_REASON, STOP_SAMPLING)
                number_sampled += 1

        for leaflet in root.get_leaves():
            if getattr(leaflet, STOP_REASON, False) != 2 and getattr(leaflet, STOP_REASON, False) != 3:
                leaflet.dist = abs(time - leaflet.DIST_TO_START)
                leaflet.add_feature(DIST_TO_START, time)
                leaflet.add_feature(STOP_REASON, STOP_TIME)

        if number_sampled == max_s:
            right_size = 1
        else:
            trial += 1

    return root


def _merge_node_with_its_child(nd, child=None, state_feature=STOP_REASON):
    if not child:
        child = nd.get_children()[0]
    nd_hist = getattr(nd, HISTORY, [(getattr(nd, state_feature, ''), 0)])
    nd_hist += [('!', nd.dist - sum(it[1] for it in nd_hist))] \
               + getattr(child, HISTORY, [(getattr(child, state_feature, ''), 0)])
    child.add_features(**{HISTORY: nd_hist})
    child.dist += nd.dist
    if nd.is_root():
        child.up = None
    else:
        parent = nd.up
        parent.remove_child(nd)
        parent.add_child(child)
    return child


def remove_certain_leaves(tre, to_remove=lambda node: False, state_feature=STOP_REASON):
    """
    Removes all the branches leading to naive leaves from the given tree.
    :param tre: the tree of interest (ete3 Tree)
    [(state_1, 0), (state_2, time_of_transition_from_state_1_to_2), ...]. Branch removals will be added as '!'.
    :param to_remove: a method to check if a leaf should be removed.
    :param state_feature: the node feature to store the state
    :return: the tree with naive branches removed (ete3 Tree) or None is all the leaves were naive in the initial tree.
    """

    for node in tre.traverse("postorder"):
        # If this node has only one child branch
        # it means that the other child branch used to lead to a naive leaf and was removed.
        # We can merge this node with its child
        # (the child was already processed and either is a leaf or has 2 children).
        if len(node.get_children()) == 1:
            merged_node = _merge_node_with_its_child(node, state_feature=state_feature)
            if merged_node.is_root():
                tre = merged_node
        elif node.is_leaf() and to_remove(node):
            if node.is_root():
                return None
            node.up.remove_child(node)
    return tre


def get_params_from_design(desi, exp_id):
    param = desi.iloc[exp_id, ]
    return param


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Generates a forest based on parameter design file. Call with: "python3 BD_simulator.py -i design_file.txt -m max_length_of_tree > output_filename.nwk "')
    parser.add_argument('-i', '--inputFile', type=str, help='an input file with parameters for the tree')
    parser.add_argument('-m', '--maxTime', type=float, help='an input float for the maximal simulation time')
    args = parser.parse_args()

    # load design file
    with open(args.inputFile, 'r') as des:
        des_data = des.read()
    des.close()

    design = pd.read_table(io.StringIO(des_data), index_col='index')

    design = design.loc[:, ['turnover_rate', 'birth_rate', 'extinction_rate', 'sampling_frac', 'tree_size']]

    # nb of trees to simulate
    nb_samples = len(design)

    # max length o ftree
    maxTime = args.maxTime

    # initialize output
    forest = []
    forest_export = pd.DataFrame(index=design.index, columns='tree')
    run_id = 0

    # simulate tree by tree
    for experiment_id in range(nb_samples):
        design = design.loc[:, ['turnover_rate', 'birth_rate', 'extinction_rate', 'sampling_frac', 'tree_size']]

        params = get_params_from_design(design, experiment_id)

        tr = simulate_bd_tree_gillespie(birth_r=params[1], extinction_r=params[2], sampling_f=params[3],
                                        max_s=params[5], max_t=maxTime)

        # for display purposes add names to nodes
        i = 0
        for node in tr.traverse("levelorder"):
            node.name = "n" + str(i)
            i += 1

        # remove extinct lineages
        tree_final = remove_certain_leaves(tr.copy(), to_remove=lambda node: getattr(node, STOP_REASON) != STOP_SAMPLING)
        if tree_final is not None:
            forest_export.iloc[run_id][0] = tree_final.write(
                features=['DIST_TO_START', 'stop_reason'], format_root_node=True,
                format=3)
        else:
            forest_export.iloc[run_id][0] = "NA"

        run_id = run_id + 1

    # export
    sys.stdout.write(forest_export.to_csv(sep='\t', index=True, header=True))
