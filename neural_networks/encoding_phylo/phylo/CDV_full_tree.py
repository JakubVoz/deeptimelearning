#!/usr/bin/env python3

import sys
import pandas as pd
import numpy as np
import argparse
import random

from ete3 import Tree

max_len = 501

TURN_ONE = 'turn_one'


DIVERSIFICATION_SCORE = 'diversification_score'

sys.setrecursionlimit(100000)


def get_average_branch_length(tre):
    """
    Returns average branch length for given tree
    :param tre: ete3.Tree, the tree on which we measure the branch length
    :return: float, average branch length
    """
    br_length = [nod.dist for nod in tre.traverse()]
    return np.average(br_length)


def rescale_tree(tr, rescale_fac):
    """
    Rescales a given tree
    :param tr: ete3.Tree, the tree to be rescaled
    :param rescale_fac: float, the branches will be multiplied by this factor
    :return: void, modifies the original tree
    """
    for node in tr.traverse():
        node.dist = node.dist/rescale_fac
    return None


def add_diversification(tr):
    """
    to each node adds an attribute, 'diversification_score', i.e. the sum of pathways of branched tips
    :param tr: ete3.Tree, the tree to be modified
    :return: void, modifies the original tree
    """
    for node in tr.traverse("postorder"):
        if not node.is_root():
            # print(label_count)
            label_node = 0
            if node.is_leaf():
                label_node = 1
                setattr(node, DIVERSIFICATION_SCORE, node.dist)
            else:
                children = node.get_children()
                # print(children)
                setattr(node, DIVERSIFICATION_SCORE, getattr(children[0], DIVERSIFICATION_SCORE) + getattr(children[1], DIVERSIFICATION_SCORE))
    return None


def add_diversification_sign(tr):
    """
    Puts topological signatures based on diversification (i.e. longest path): if the first child of a node has longer
    path of branches leading to it, then it is prioritized for visit.
    :param tr: ete3.Tree, the tree to get the topological description
    :return: void, modifies the original tree
    """
    for node in tr.traverse('levelorder'):
        if not node.is_leaf():
            diver_child0 = getattr(node.children[0], DIVERSIFICATION_SCORE)
            diver_child1 = getattr(node.children[1], DIVERSIFICATION_SCORE)
            if diver_child0 < diver_child1:
                node.add_feature(TURN_ONE, True)
            elif diver_child0 == diver_child1:
                next_sign = random.choice([True, False])
                if next_sign is True:
                    node.add_feature(TURN_ONE, True)
            else:
                node.add_feature(TURN_ONE, False)
    return None


def name_tree(tr):
    """
    Names all the tree nodes that are not named, with unique names.
    :param tr: ete3.Tree, the tree to be named
    :return: void, modifies the original tree
    """
    i = 0
    for node in tr.traverse('levelorder'):
        node.name = i
        i += 1
    return None


def add_dist_to_root(tr):
    # int_nodes_dist = []
    # tips_dist = []
    tree_height = 0
    for node in tr.traverse("preorder"):
        if node.is_root():
            node.add_feature("dist_to_root", 0)
        elif node.is_leaf():
            node.add_feature("dist_to_root", getattr(node.up, "dist_to_root") + node.dist)
            # tips_dist.append(getattr(node.up, "dist_to_root") + node.dist)
            tree_height = getattr(node, "dist_to_root", False)

        else:
            node.add_feature("dist_to_root", getattr(node.up, "dist_to_root") + node.dist)
            # int_nodes_dist.append(getattr(node.up, "dist_to_root") + node.dist)
    return tr, tree_height


def get_not_visited_anc(leaf):
    while getattr(leaf, "visited", False):
        leaf = leaf.up
    return leaf


def get_dist_to_root(anc):
    dist_to_root = getattr(anc, "dist_to_root")
    return dist_to_root


def follow_signs(anc):
    end_leaf = anc
    while not end_leaf.is_leaf():
        if getattr(end_leaf, TURN_ONE, False):
            if getattr(end_leaf.children[1], 'visited', False):
                end_leaf = end_leaf.children[0]
            else:
                end_leaf = end_leaf.children[1]
        else:
            if getattr(end_leaf.children[0], 'visited', False):
                end_leaf = end_leaf.children[1]
            else:
                end_leaf = end_leaf.children[0]
    return end_leaf


def enc_diver(anc):
    leaf = follow_signs(anc)
    setattr(leaf, 'visited', True)
    anc = get_not_visited_anc(leaf)
    if anc is None:
        # print("what")
        return
    setattr(anc, 'visited', True)
    yield get_dist_to_root(anc)
    for _ in enc_diver(anc):
        yield _


def complete_coding(encoding, max_length):
    add_vect = np.repeat(0, max_length - len(encoding))
    add_vect = list(add_vect)
    encoding.extend(add_vect)
    return encoding


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Encodes tree into full tree representation. Call script from terminal with: python3 CDV_full_tree.py -t ./filename.nwk >> encoded_full_tree.csv')
    parser.add_argument('-t', '--tree', type=str, help='name of the file with nwk trees')
    args = parser.parse_args()

    # read nwk file with trees
    tree = str(args.tree)
    file = open(tree, mode="r")
    forest = file.read().replace("\n", "")
    trees = forest.split(";") # split to individual trees

    # encode tree by tree
    for i in range(0, len(trees)):

        if len(trees[i]) > 0:
            tree = Tree(trees[i] + ";", format=1)
            name_tree(tree)

            # rescale tree to average branch length of 1
            # measure average branch length
            rescale_factor = get_average_branch_length(tree)
            # rescale tree
            rescale_tree(tree, rescale_factor)

            # add dist to root attribute
            tree, tr_height = add_dist_to_root(tree)

            # add pathway of visiting priorities for encoding
            add_diversification(tree)
            add_diversification_sign(tree)

            # encode the tree
            tree_embedding = list(enc_diver(tree))

            # add tree height
            tree_embedding.insert(0, tr_height)

            # complete embedding
            tree_embedding = complete_coding(tree_embedding, max_len)

            # add type count and scaling factor
            tree_embedding.extend([rescale_factor])

            line_DF = pd.DataFrame(tree_embedding, columns=[i])

            if i == 0:
                result = line_DF
            else:
                result = pd.concat([result, line_DF], axis=1)

    result = result.T

    sys.stdout.write(result.to_csv(sep='\t', index=True, index_label='Index'))
