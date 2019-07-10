module TWTypes where

import PropLogic 

type Node = Int
type Edge = (Node, Node)
type NodePairF = Node -> Node -> Variable
type BitF = Bit -> Node -> Variable 
type Bit = Int
type BitPairF = Bit -> Node -> Node -> Variable

