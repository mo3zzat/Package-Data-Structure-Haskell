module DataStructure
(Tree(..)
,avl_createNode
,avl_depth
,avl_isempty
,avl_isfindintree
,avl_checkblanced
,avl_in_order
,avl_pre_order
,avl_post_order
,avl_min_value_intree
,avl_max_value_intree
,avl_sum_value_intree
,avl_size_tree
,avl_sorttree
,avl_getleft
,avl_getright
,avl_getrootvalue
,avl_takesubtree_onvalue
,avl_rotate_tree
,avl_insertintree
,avl_insert_list_totree
,avl_insert_list_to_newtree
,avl_delete
,avl_delete_list_fromtree
,avl_blance
,G(..)
,g_getNode
,g_allVertex
,g_lstNode2Vertex
,g_lstVertex2Node
,g_createGraph
,g_checkGraph
,g_edgeInGraph
,g_nodeInGraph
,g_isSelfLoop
,g_appendNode
,g_appendEdge
,g_creategraph_fromlstofV
,g_appendlstNd_onGraph
,g_lstNdEmptyGraph
,g_removeEdge
,g_adjacentList
,g_lstEdCrtGraph
,g_breadthFsttSearch
,g_depthFstSearch
,g_mySort
,g_mst
,HashTable(..)
,hash_printhashtable
,hash_isEmpty
,hash_sizeofHash
,hash_getElementsinBucket
,hash_insertHashTableKey
,hash_removeKey
,hash_updateHashKey
,hash_searchHash
) where
import Data.List as List
import Data.Char
--------------------------------------------------AVL_tree------------------------

data Tree t = Nul | Node t (Tree t) (Tree t)
          deriving (Eq,Ord,Read)
instance Show t => Show (Tree t) where
 show Nul = "Nul"
 show (Node v lft right) = "(" ++ show lft ++ " " ++ show v ++ " " ++ show right ++ ")"

-----------------------------some_tree_to_test_code----------------------------------

t1=(Node 70 (Node 30 (Node 20 Nul Nul)Nul)Nul)
t2=(Node 80 (Node 70(Node 60(Node 50 Nul Nul) Nul)Nul) (Node 90 (Node 95(Node 100 Nul Nul)Nul) Nul))
t3=(Node 44 (Node 10 (Node 1 Nul (Node 2 Nul Nul)) (Node 30 (Node 15 Nul Nul) Nul)) (Node 99 (Node 66 (Node 50 Nul Nul) (Node 70 Nul Nul)) (Node 500 (Node 102 Nul Nul) Nul)))

-------------------------------------------------------------------------------------
avl_createNode:: (Ord t, Num t) => t ->Tree t
avl_createNode v =(Node v Nul Nul)

avl_isempty :: (Ord t, Num t)  => Tree t -> Bool
avl_isempty Nul = True
avl_isempty  _  = False

avl_depth :: (Ord t, Num t) => Tree t -> t
avl_depth Nul = -1
avl_depth (Node v lft right) = 1 + (max (avl_depth lft) (avl_depth right))

---------------------------------------------some functions in  Tree--------------------------------------- 

avl_sum_value_intree::(Ord t, Num t) => Tree t -> t
avl_sum_value_intree Nul =0
avl_sum_value_intree (Node v lft right) = v + (avl_sum_value_intree right)+(avl_sum_value_intree lft)

avl_min_value_intree::(Ord t, Num t) => Tree t -> t
avl_min_value_intree (Node v Nul _ ) = v
avl_min_value_intree  (Node v lft right) = avl_min_value_intree lft

avl_max_value_intree::(Ord t, Num t) => Tree t -> t
avl_max_value_intree (Node v _ Nul ) = v
avl_max_value_intree  (Node v lft right) = avl_max_value_intree right

avl_size_tree::(Ord t, Num t) => Tree t -> t
avl_size_tree Nul =0
avl_size_tree (Node v lft right) = 1 + (avl_size_tree lft) +(avl_size_tree right)

-------------------------------------------------------------------------------------
avl_checkblanced :: (Ord t, Num t) => Tree t -> Bool
avl_checkblanced Nul = True
avl_checkblanced  (Node v lft right) | not (avl_checkblanced right) = False 
                                     | not (avl_checkblanced lft) = False
                                     | abs ((avl_depth lft) - (avl_depth right)) > 1 = False
                                     | otherwise = True

-------------------------------------------------------------------------------------

avl_getleft::(Ord t, Num t) => Tree t ->Tree t
avl_getleft Nul =Nul
avl_getleft(Node v lft right)=lft

avl_getright::(Ord t, Num t) => Tree t ->Tree t
avl_getright Nul =Nul
avl_getright(Node v lft right)=right

avl_getrootvalue ::(Ord t, Num t) => Tree t ->t
avl_getrootvalue Nul = 0
avl_getrootvalue (Node v lft right)=v

avl_takesubtree_onvalue :: (Ord t, Num t) => t-> Tree t->Tree t
avl_takesubtree_onvalue e Nul =Nul
avl_takesubtree_onvalue e (Node v lft right)  |v == e    = Node v lft right
                                              |v < e     = avl_takesubtree_onvalue e right
                                              |otherwise = avl_takesubtree_onvalue e lft

avl_isfindintree ::(Ord t, Num t) => Tree t -> t -> Bool
avl_isfindintree Nul _ =False
avl_isfindintree  (Node v lft right) x |v == x  =True
                                       |x < v   = avl_isfindintree lft x
                                       |x > v   =avl_isfindintree  right x                                              
-------------------type_of_rotation------------------------------------------------------------------

left_left :: (Ord t, Num t) => Tree t->Tree t
left_left (Node v l r) =Node (avl_getrootvalue l) (avl_getleft l) (Node v (avl_getright l) r)

right_right:: (Ord t, Num t) => Tree t->Tree t
right_right (Node v l r)=Node (avl_getrootvalue r) (Node v l (avl_getleft r)) (avl_getright r)


right_left:: (Ord t, Num t) => Tree t->Tree t
right_left (Node v l r)= Node (avl_getrootvalue (avl_getleft r)) (Node v l (avl_getleft (avl_getleft r))) (Node (avl_getrootvalue r) (avl_getright (avl_getleft r)) (avl_getright r))

left_right :: (Ord t, Num t) => Tree t->Tree t
left_right (Node v l r)= Node (avl_getrootvalue (avl_getright l)) (Node (avl_getrootvalue l) (avl_getleft l) (avl_getleft (avl_getright l))) (Node v (avl_getright (avl_getright l)) r)


------------------------------------------insertion---------------------------------------------------------

avl_insertintree :: (Ord t, Num t) => t-> Tree t->Tree t
avl_insertintree e Nul =avl_createNode e
avl_insertintree e (Node v lft right)  |v == e    =avl_rotate_tree (Node  e lft right)
                                       |v < e     = avl_rotate_tree (Node v lft (avl_insertintree e right))
                                       |otherwise = avl_rotate_tree (Node v  (avl_insertintree e lft) right)
 
avl_rotate_tree :: (Ord t, Num t) => Tree t -> Tree t
avl_rotate_tree Nul = Nul
avl_rotate_tree (Node v l r) | not (avl_checkblanced l) = Node v (avl_rotate_tree l) r
                             | not (avl_checkblanced r) = Node v l (avl_rotate_tree r)

                             | (avl_depth r) > (avl_depth l) + 1  && (avl_depth (avl_getleft r))  < (avl_depth (avl_getright r))  = right_right (Node v l r)     -- SR_RR

                             | (avl_depth l) > (avl_depth r) + 1  && (avl_depth (avl_getright l)) < (avl_depth (avl_getleft l))   = left_left   (Node v l r)    -- SR_LL

                             | (avl_depth r) > (avl_depth l) + 1  && (avl_depth (avl_getleft r))  >  (avl_depth (avl_getright r)) = right_left  (Node v l r)    -- DR_RL

                             | (avl_depth l) > (avl_depth r) + 1  && (avl_depth (avl_getright l)) > (avl_depth (avl_getleft l))   = left_right  (Node v l r)    -- DR_LR

                             | otherwise = Node v l r


avl_insert_list_totree :: (Ord t, Num t) =>Tree t -> [t] -> Tree t
avl_insert_list_totree t [] = t
avl_insert_list_totree t (x:xs)= avl_insertintree x (avl_insert_list_totree t xs)


avl_insert_list_to_newtree:: (Ord t, Num t) => [t] -> Tree t
avl_insert_list_to_newtree []= Nul
avl_insert_list_to_newtree (x:xs) = avl_insertintree x (avl_insert_list_totree Nul xs)

--------------------------------------------------------deletion------------------------------------------------
avl_delete ::(Ord t, Num t) => t->Tree t -> Tree t
avl_delete _ Nul  = Nul
avl_delete e (Node v lft right)   | v > e = avl_rotate_tree((Node v (avl_delete e lft )  right))
                                  | v < e = avl_rotate_tree((Node v lft (avl_delete e right)))
                                  | e == v = avl_rotate_tree(delet_root_node (Node v lft right))

--------------------------- delete_root_from_this_node
delet_root_node :: (Ord t, Num t) =>Tree t -> Tree t
delet_root_node (Node _ Nul right) = right
delet_root_node (Node _ lft Nul)   = lft
delet_root_node (Node v lft right) = (Node v' (avl_delete v' lft) right)
                                           where
                                            v' = lastright_left lft


------------------------------if_node_have_left&&right_retrun_last right in left
lastright_left :: (Ord t, Num t) =>Tree t -> t
lastright_left (Node v _ Nul) = v
lastright_left (Node _ _ right) = lastright_left right

avl_delete_list_fromtree :: (Ord t, Num t) =>Tree t -> [t] -> Tree t
avl_delete_list_fromtree t [] = t
avl_delete_list_fromtree t (x:xs)= avl_delete x (avl_delete_list_fromtree t xs)

-------------------------------Macke_unbalnced_tree==>balanced-------------------------------------------
avl_blance::(Ord t, Num t) => Tree t -> Tree t 
avl_blance Nul=Nul
avl_blance (Node v lft right)=avl_rotate_tree (Node v (avl_blance lft)  (avl_blance right))

----------------------------------------Traversal-----------------------------------------------------------
avl_in_order :: (Ord t, Num t) => Tree t -> [t]
avl_in_order Nul=[]
avl_in_order (Node v lft Nul)    = avl_in_order lft ++ [v]
avl_in_order (Node v Nul right)  = [v] ++ avl_in_order right
avl_in_order (Node v lft right)  = avl_in_order lft ++ [v] ++avl_in_order right

avl_pre_order ::(Ord t, Num t) => Tree t -> [t]
avl_pre_order Nul= []
avl_pre_order (Node v lft right) = [v] ++ avl_pre_order lft ++ avl_pre_order right

avl_post_order ::(Ord t, Num t) => Tree t -> [t]
avl_post_order Nul= []
avl_post_order (Node v lft right) = avl_post_order lft ++ avl_post_order right ++ [v]

avl_sorttree::(Ord t, Num t) => Tree t -> [t] 
avl_sorttree Nul =[]
avl_sorttree t =avl_in_order t

----------------------------------------fina_lly _ avl tree--------------------------------------------------

-----------------------------------------------graph-----------------------------------------
type V = Int
type Values = String
type Weigth = Int
type Ed = ((V, V),Weigth)
type Nd = (V, Values)
type G = ([Nd], [Ed])

g1:: G
g1= ([(1,"1"),(2,"2"),(3,"3"),(4,"4"),(5,"5"),(6,"6")],[((1,2),3),((1,5),4),((1,6),7),((2,6),8),((2,3),5),((3,6),6),((3,4),4),((4,5),2),((4,6),8),((5,6),5)])
g2:: G
g2= ([(1,"s"),(2,"s1"),(3,"s2"),(4,"s3"),(5,"s4"),(6,"s5"),(7,"7"),(8,"8")],[((1,2),3),((1,3),4),((2,4),5),((2,5),6),((2,6),7),((3,4),1),((4,5),6),((4,7),6),((5,6),1),((6,7),3),((6,8),1),((7,8),3)])
-------------------------------------------------------------------------------------
g_allVertex :: G -> [V]
g_allVertex g = [fst n | n<- fst g] 

g_lstNode2Vertex :: [Nd] -> [V]
g_lstNode2Vertex [] = []
g_lstNode2Vertex ns = [v | (v , value) <- ns]

g_getNode :: V -> G -> Nd
g_getNode v (((x,y):ns) , es)
                              | v == x = (x,y)
                              | otherwise = g_getNode v (ns , es)

g_lstVertex2Node :: [V] -> G -> [Nd]
g_lstVertex2Node [] _ = []
g_lstVertex2Node vs g = [g_getNode v g | v <- vs]

g_edgeInGraph :: Ed -> G -> Bool
g_edgeInGraph ((u, v),z) g = sum([1 | ((x , y),w) <- snd g , u==x && v==y && z==w])>0
------------------------------------Creat----------------------------------------------
g_nodeInGraph :: V -> G -> Bool
g_nodeInGraph _ ([], _) = False
g_nodeInGraph v g = sum([1 | x <- g_allVertex g , x==v ]) > 0 

g_isSelfLoop :: Ed -> G -> Bool
g_isSelfLoop ((u, v),z) g = sum([1 | ((x,y),w) <- snd g , u==x && v==y])>0

g_createGraph :: G
g_createGraph = ([], [])

g_checkGraph :: G -> Bool
g_checkGraph ([], _) = True
g_checkGraph g = False
-------------------------------------Append---------------------------------------------------------------
g_appendNode :: G -> Nd -> G
g_appendNode g (x,y)
                    | g_nodeInGraph x g = g
                    | otherwise = ((x,y):(fst g), snd g)

g_appendEdge :: G -> Ed -> G
g_appendEdge g ((u, v),z) 
                    | (g_nodeInGraph u g) && (g_nodeInGraph v g) && (not(g_edgeInGraph ((u, v),z) g)) = (fst g, ((u, v),z):(snd g))
                    | otherwise = g
-----------------------------------create graph---------------------------------------------------------------
g_creategraph_fromlstofV :: [V] -> G
g_creategraph_fromlstofV [] = g_createGraph
g_creategraph_fromlstofV v = g_appendNode (g_creategraph_fromlstofV (tail v)) (head v, "")  

g_appendlstNd_onGraph :: G -> [Nd] -> G
g_appendlstNd_onGraph g = foldl g_appendNode g

g_lstNdEmptyGraph :: [Nd] -> G
g_lstNdEmptyGraph = foldl g_appendNode g_createGraph

g_lstEdCrtGraph :: G -> [Ed] -> G
g_lstEdCrtGraph g = foldl g_appendEdge g
--------------------------------------------Remove------------------------------------------------------
g_removeNode :: V -> G -> G
g_removeNode v g
                    | (g_nodeInGraph v g) && not (any (\((x, y),w) -> x == v || y == v) (snd g)) = (deleteNode v (fst g), snd g)
                    | otherwise = g
                    where deleteNode u xs = if (fst (head xs)) == u then tail xs else head xs : deleteNode u (tail xs)

g_removeEdge :: Ed -> G -> G
g_removeEdge ((u, v),w) g
                   | g_nodeInGraph u g && g_nodeInGraph v g && g_edgeInGraph((u, v),w) g = (fst g, deleteE u v (snd g))
                   | otherwise = g
                   where deleteE z k xs = if ((fst (fst (head xs)) == z) && (snd (fst (head xs)) == k)) then tail xs else head xs : deleteE z k (tail xs)
------------------------------------------------adjacent--------------------------------------------
g_adjacentList :: V -> G -> [Nd]
g_adjacentList v g
                  | g_nodeInGraph v g = g_lstVertex2Node (adjacentN v (snd g)) g
                  | otherwise = []
                  where adjacentN v [] = []
                        adjacentN v (((x,y),w) : xs)
                          | x == v = y : adjacentN v xs
                          | otherwise = adjacentN v xs

bfsAndDfs:: [V]->[Nd]->[Nd]
bfsAndDfs vs ns =[n | n<-ns , not(elem (fst n) vs ) ]

-------------------------BradthFirstSearch Function-------------------------------------
g_breadthFsttSearch :: V -> G -> [Nd]
g_breadthFsttSearch v g
                  | g_nodeInGraph v g = if (null(g_adjacentList v g)) then [g_getNode v g] else b_f_s [v] [g_getNode v g] g
                  | otherwise = []

b_f_s::[V]->[Nd]->G->[Nd]
b_f_s _ [] _ = []
b_f_s vs ((x,y):ns) g = (x,y) : b_f_s (vs ++ [x]) (queueNodes (x) (vs ++ [x]) (g_adjacentList (x) g) ns) g

queueNodes::V->[V]->[Nd]->[Nd]->[Nd]
queueNodes v ns [] xs = xs
queueNodes v ns vs xs = xs ++ (filter (\x -> (not (any (\y->x == y || v == (fst x)) xs))) (bfsAndDfs ns vs ))
-------------------------------depth First Search------------------------------------------------------
g_depthFstSearch :: V -> G -> [Nd]
g_depthFstSearch v g
                  | g_nodeInGraph v g = if (null (g_adjacentList v g)) then [g_getNode v g] else d_f_s [v] [g_getNode v g] g
                  | otherwise = []

d_f_s::[V]->[Nd]->G->[Nd]
d_f_s _ [] _ = []
d_f_s vs ((x,y):ns) g = (x,y) : d_f_s (vs ++ [x]) (stackNodes (x) (vs ++ [x]) (g_adjacentList (x) g) ns) g

stackNodes::V->[V]->[Nd]->[Nd]->[Nd]
stackNodes v ns [] xs = xs
stackNodes v ns vs xs = (filter (\x-> (not(any (\y -> x == y || v == (fst x)) xs))) (bfsAndDfs ns vs)) ++ xs
------------------------Sort for MST---------------------------------------------------------------------
g_mySort::[Ed]->[Ed]
g_mySort [] = []
g_mySort [((x,y),z)] = [((x,y),z)]
g_mySort (((x,y),z):xs) = ins ((x,y),z) (g_mySort xs)

ins::Ed->[Ed]->[Ed]
ins ((x,y),z) [] =[((x,y),z)]
ins ((x,y),z) (((u,v),w):xs)
                   |z<=w = ((x,y),z) : (((u,v),w) : xs)
                   |otherwise = ((u,v),w) : ins ((x,y),z) xs
--------------MST---------------------
g_mst::G -> ([[V]],[Ed])
g_mst g = allnode (g_mySort (snd g)) ((getlstoflstfromnod (fst g)),[])

getlstoflstfromnod::[Nd]->[[V]]
getlstoflstfromnod ns = [[fst n] | n<-ns]

allnode::[Ed]->([[V]],[Ed])->([[V]],[Ed])
allnode []  g  = g
allnode (m:ms) (node,edge) 
                          | inSameLst m node  = allnode ms (node,edge)
                          | otherwise = allnode ms ((dolist m node,mst' m edge))

inSameLst::Ed->[[V]]->Bool   
inSameLst ((x,y),z) [] = False
inSameLst ((x,y),z) (m:xs) 
                          |elem x m && elem y m = True
                          |otherwise = inSameLst ((x,y),z)  xs

dolist::Ed->[[V]]->[[V]]
dolist ((x,y),z) [] =[[x,y]]
dolist ((x,y),z) ms =[makelist ([m | m<-ms , elem x m] ++  [m | m<-ms , elem y m])] ++ [m | m<-ms , not (elem y m || elem x m)]

mst'::Ed->[Ed]->[Ed]
mst' m xs = m:xs

makelist::[[V]]->[V]
makeList [] =[]
makelist xs = [y | x <- xs, y <- x]
------------------------------------------------finally-graph---------------------




------------------------------------------------hash_table-----------------------


type HashTable a = [Bucket a]
type Bucket a = [(String,a)]

hash = [[("Mario",20170396),("Hamada",20170397)],[],[("Joe",20170460)],[("Michael",20170510)]]
hash2 = [[("Mario","Male")],[],[],[("Sarah","Female")]]
hash3 = [[],[],[],[],[],[],[],[],[]]

---------------------------------------------------------
hash_printhashtable [] = []
hash_printhashtable (x:xs) = "[ "++(hash_printhashtablebuckets x++ "  " ) ++ hash_printhashtable xs

hash_printhashtablebuckets [] =[]
hash_printhashtablebuckets (x:xs) = ( show x  ++ ",")++hash_printhashtablebuckets xs ++ " ]"

hash_isEmpty::HashTable a->Bool
hash_isEmpty [] = True
hash_isEmpty table = False

hash_sizeofHash:: HashTable a->Int
hash_sizeofHash [] = 0
hash_sizeofHash table =lengthlist table

lengthlist:: [a] -> Int
lengthlist [] = 0
lengthlist (x:xs) = 1 + lengthlist xs 


hash_getElementsinBucket::HashTable a->Int->Bucket a
hash_getElementsinBucket [] _ = []
hash_getElementsinBucket table index = table!!index
--------------------------------------------------------------------------------------
hash_removeKey :: HashTable a -> String -> HashTable a
hash_removeKey table key = tables
  where
    tables = leftpart ++ [item] ++ rightpart
    item = filter (removeKeyFilter key) bucket
    (leftpart,(_:rightpart)) = splitAt index table
    bucket = table !! index
    index = getIndex table key

removeKeyFilter :: String->(String,a)->Bool
removeKeyFilter searchkey (key,value) = key /= searchkey
---------------------------------------------------------------------------------------
hash_insertHashTableKey :: HashTable a -> String -> a -> HashTable a
hash_insertHashTableKey table key value = inserthashKeyAUX table bucket key value
                     where
                     bucket = table !! index
                     index = getIndex table key

inserthashKeyAUX :: HashTable a->Bucket a -> String -> a -> HashTable a
inserthashKeyAUX table [] key value = insertList table index [(key,value)]
                     where
                     index = getIndex table key
inserthashKeyAUX table xs key value = insertList table index ((key,value):xs)
                     where
                     index = getIndex table key

insertList :: HashTable a -> Int -> Bucket a -> HashTable a
insertList table index (x:xs) = tables
  where
    tables = leftpart ++ [item] ++ rightpart
    item = x:uniqueKeys
    uniqueKeys = filter (insertfilter (fst x)) xs
    (leftpart,deleted:rightpart) = splitAt index table

insertfilter:: String->(String,a)->Bool
insertfilter searchkey (key,_)  = key/=searchkey
--------------------------------------------------------------------------------------
hash_updateHashKey :: HashTable a -> String -> a -> HashTable a
hash_updateHashKey table key value = updateHashKeyAUX table bucket key value
                     where
                     bucket = table !! index
                     index = getIndex table key

updateHashKeyAUX :: HashTable a->Bucket a -> String -> a -> HashTable a
updateHashKeyAUX table xs key value = updateList table index ((key,value):xs)
                     where
                     index = getIndex table key

updateList :: HashTable a -> Int -> Bucket a -> HashTable a
updateList table index (x:xs) = tables
  where
    tables = leftpart ++ [item] ++ rightpart
    item = x:uniqueKeys
    uniqueKeys = filter (updatefilter (fst x)) xs
    (leftpart,(removed:rightpart)) = splitAt index table

updatefilter:: String->(String,a)->Bool
updatefilter searchkey (key,_)  = key/=searchkey

---------------------------------------------------------------------------------------

hash_searchHash :: HashTable a -> String -> Maybe a
hash_searchHash table key = searchHashaux (table!!(getIndex table key)) key

searchHashaux:: Bucket a -> String -> Maybe a
searchHashaux [] key = Nothing
searchHashaux xs key = Just call
                where call = findInBucket key xs

findInBucket :: String -> Bucket a -> a
findInBucket searchKey xs = value
  where
    (key,value) = head (filter (findInBucketfilter searchKey) xs)


findInBucketfilter :: String->(String,a)->Bool
findInBucketfilter searchkey (key,value) = key == searchkey

-------------------------------------------------------------------------

getIndex :: HashTable a -> String -> Int
getIndex table key = ((calculateHash key) `mod` (lengthlist table))

calculateHash :: String -> Int
calculateHash [] = 0
calculateHash (x:xs) = ord(x) + calculateHash xs

---------------------------finaaally-------------------------------------------