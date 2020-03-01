# pert
[PERT (Program Evaluation and Review Technique)](https://en.wikipedia.org/wiki/Program_evaluation_and_review_technique) CSV => DOT language (able to be processed with GraphViz) tool

## SYNOPSYS

This tool generates DOT document from CSV document.

CSV represents PERT dependency graph.

CSV consists of 4 column: `from`, `to`, `cost`, `name`. Header is mandatory.

For example:

```csv
from,to,cost,name
1,2,4,"Read Object-Oriented Software Construction"
1,3,1,"Read Readable Code"
1,4,2,"Read Hackers and Painters"
2,5,0,""
3,5,0,""
4,5,0,""
5,6,2,"Buy MacBook"
6,7,2,"Become superhacker"
```

This CSV is converted with `sbt run foobar.csv`:

```
digraph PERT {

graph [
rankdir = "LR"
];
    
Ev2 [ shape = circle, label = "2\n4.0..4.0" ];
Ev5 [ shape = circle, label = "5\n4.0..4.0" ];
Ev7 [ shape = circle, label = "7\n8.0..8.0" ];
Ev6 [ shape = circle, label = "6\n6.0..6.0" ];
Ev1 [ shape = circle, label = "1\n0.0..0.0" ];
Ev4 [ shape = circle, label = "4\n2.0..4.0" ];
Ev3 [ shape = circle, label = "3\n1.0..4.0" ];
Ev2 -> Ev5 [ style = dashed, label = "(0.0)\n[T:0.0/F:0.0]" ];
Ev1 -> Ev4 [ label = "Read Hackers and Painters(2.0)\n[T:2.0/F:0.0]" ];
Ev1 -> Ev3 [ label = "Read Readable Code(1.0)\n[T:3.0/F:0.0]" ];
Ev6 -> Ev7 [ style = bold, label = "Become superhacker(2.0)\n[T:0.0/F:0.0]" ];
Ev3 -> Ev5 [ style = dashed, label = "(0.0)\n[T:3.0/F:3.0]" ];
Ev1 -> Ev2 [ style = bold, label = "Read Object-Oriented Software Construction(4.0)\n[T:0.0/F:0.0]" ];
Ev4 -> Ev5 [ style = dashed, label = "(0.0)\n[T:2.0/F:2.0]" ];
Ev5 -> Ev6 [ style = bold, label = "Buy MacBook(2.0)\n[T:0.0/F:0.0]" ];
}
```

You can process DOT document with graphviz:

![](https://github.com/windymelt/pert/blob/master/image.png)

You see that `1->2->5->6->7` is *critical path* shown in bold edge.

## Legends

### Edge

- `ACTIVITY_NAME(n) [T:t/F:f]`
  - `ACTIVITY_NAME`: activity name.
  - `n`: cost for activity.
  - `t`: total float.
  - `f`: free float.
- dashed edge
  - means dummy node (cost is 0).
- bold edge
  - means critical path.
- red bold edge
  - needs shortening in this path (priority).
- red edge
  - needs shortening in this path.
  
### Node

- `n a..b`
  - `n`: event no..
  - `a`: earliest arrival time.
  - `b`: deadline finish time.
