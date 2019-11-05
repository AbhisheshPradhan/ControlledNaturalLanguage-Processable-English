class(1,node).

data_prop(1,pos_int(1),cardinal).

prop(1,2,connected_to).

class(2,node).

data_prop(2,pos_int(2),cardinal).

prop(1,3,connected_to).

class(3,node).

data_prop(3,pos_int(3),cardinal).

prop(1,4,connected_to).

class(4,node).

data_prop(4,pos_int(4),cardinal).

prop(2,4,connected_to).

prop(2,5,connected_to).

class(5,node).

data_prop(5,pos_int(5),cardinal).

prop(2,6,connected_to).

class(6,node).

data_prop(6,pos_int(6),cardinal).

prop(3,1,connected_to).

prop(3,4,connected_to).

prop(3,5,connected_to).

prop(4,1,connected_to).

prop(4,2,connected_to).

prop(5,3,connected_to).

prop(5,4,connected_to).

prop(5,6,connected_to).

prop(6,2,connected_to).

prop(6,3,connected_to).

prop(6,5,connected_to).

named(7,red).

class(7,colour).

named(8,blue).

class(8,colour).

named(9,green).

class(9,colour).

1 { prop(A,B,assigned_to) : class(B,colour) } 1 :- class(A,node).

