import os
import json
import sys
import time
import pathlib
import numpy as np


# --------------- Set up TF_CONFIG --------------- #
# /home/hardyxu/Compute_Request/nodes.txt
# load the index and node info from the nodes.txt file
# open the file and read the lines
with open('c:/Users/Hardy/Documents/MIT/Thesis/Code/Github/cnn_traits_dispersal/code/nodes.txt') as f:
    lines = f.readlines()
# First line looks like this: node[029,279]
# Get the node numbers and convert to a list of ints
nodes = [int(x) for x in lines[0].split('[')[1].split(']')[0].split(',')]
tf_config = {
        'cluster': {
            'worker': []
        },
        'task': {'type': 'worker', 'index': 0}
    }
# node names for each node, append a port # to the names
for i in range(len(nodes)):
    tf_config['cluster']['worker'].append('node[' + nodes[i] + ']:2001')
    tf_config['task']['index'] = i
os.environ['TF_CONFIG'] = json.dumps(tf_config)
print(os.environ['TF_CONFIG'])