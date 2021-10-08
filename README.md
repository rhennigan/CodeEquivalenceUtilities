# CodeEquivalenceUtilities

<table>
<tr>
<td>
CodeEquivalenceUtilities is a collection of Wolfram Language functions that can be used to test if different pieces of code are equivalent without the need for evaluation.

This allows comparison of unevaluated expressions that may have non-deterministic outputs (e.g. random values, dates, etc).

This Paclet represents the underlying technology that powers several automated code grading systems, such as the online exercises for EIWL and Wolfram Challenges.
</td>
<td>
<img src="https://www.wolframcloud.com/obj/resourcesystem/published/PacletRepository/resources/Wolfram-CodeEquivalenceUtilities/hero-image.png" width="50%">
</td>
</tr>
</table>

[![View notebooks](https://wolfr.am/HAAhzkRq)](https://wolfr.am/Z72fzrAk)

## Installing CodeEquivalenceUtilities


### From the Wolfram Paclet Repository

```Mathematica
(* Coming soon... *)
PacletInstall[ResourceObject["Wolfram: CodeEquivalenceUtilities"]]
```

### Using [GitHubInstall](https://resources.wolframcloud.com/FunctionRepository/resources/GitHubInstall/)

```Mathematica
ResourceFunction["GitHubInstall"]["rhennigan", "CodeEquivalenceUtilities"]
```

### From Github
The CodeEquivalenceUtilities release comes in the form of a `.paclet` file, which contains the entire package and its documentation. Download the latest release from the [GitHub repo's releases page](https://github.com/rhennigan/CodeEquivalenceUtilities/releases). To install, run the following command in the Wolfram Language:

```Mathematica
PacletInstall["/full/path/to/CodeEquivalenceUtilities.paclet"]
```

This will permanently install the CodeEquivalenceUtilities paclet. The Wolfram Language will always use the latest installed version of CodeEquivalenceUtilities. Installed versions can be enumerated using the command:

```Mathematica
PacletFind["Wolfram__CodeEquivalenceUtilities"]
```

And all versions can be uninstalled using the command:

```Mathematica
PacletUninstall["Wolfram__CodeEquivalenceUtilities"]
```

## Paclet Guide

Equivalence for Wolfram Language code can be defined in many ways. The methods used by CodeEquivalenceUtilities attempt to determine intensional equivalence by transforming expressions into a canonical representation.

### Equivalence Testing

[CodeEquivalentQ](https://www.wolframcloud.com/obj/resourcesystem/published/PacletRepository/resources/Wolfram-CodeEquivalenceUtilities/ref/CodeEquivalentQ.html) - test if two unevaluated expressions are equivalent

[EquivalenceTestData](https://www.wolframcloud.com/obj/resourcesystem/published/PacletRepository/resources/Wolfram-CodeEquivalenceUtilities/ref/EquivalenceTestData.html) - get additional information about the equivalence test performed by 
[CodeEquivalentQ](https://www.wolframcloud.com/obj/resourcesystem/published/PacletRepository/resources/Wolfram-CodeEquivalenceUtilities/ref/CodeEquivalentQ.html)

### Code Transformation

[ToCanonicalForm](https://www.wolframcloud.com/obj/resourcesystem/published/PacletRepository/resources/Wolfram-CodeEquivalenceUtilities/ref/ToCanonicalForm.html) - convert an expression into a canonical representation for direct comparison

[MakeCanonicalForm](https://www.wolframcloud.com/obj/resourcesystem/published/PacletRepository/resources/Wolfram-CodeEquivalenceUtilities/ref/MakeCanonicalForm.html) - convert to canonical form without evaluating the input

[FromCanonicalForm](https://www.wolframcloud.com/obj/resourcesystem/published/PacletRepository/resources/Wolfram-CodeEquivalenceUtilities/ref/FromCanonicalForm.html) - convert a canonical representation back into a normal evaluatable expression

## Examples

### Basic Examples (2)

Check if two expressions are equivalent:

```Mathematica
In[1]:= CodeEquivalentQ[RandomInteger /@ Range[5], Array[RandomInteger, 5]]

Out[1]= True
```

---

View the canonical representations of expressions:

```Mathematica
In[1]:= MakeCanonicalForm[RandomInteger /@ Range[5]]

Out[1]= Table[R[DiscreteUniformDistribution[{0, S1::Int}]],{S1::Int, 1, 5, 1}]
```

```Mathematica
In[2]:= MakeCanonicalForm[Array[RandomInteger, 5]]

Out[2]= Table[R[DiscreteUniformDistribution[{0, S1::Int}]], {S1::Int, 1, 5, 1}]
```

These are directly comparable:

```Mathematica
In[3]:= % === %%

Out[3]= True
```

### Scope (3)

Get additional information about the equivalence test:

```Mathematica
In[1]:= EquivalenceTestData[
            First[Rest[Range /@ Range[2^100]]],
            Part[Table[Table[j, {j, i}], {i, 2^100}], 2]
        ]

Out[1]= <|"Timing" -> <|"SameQ" -> 0.*10^-8, "ToCanonicalForm1" -> 0.9639954, 
            "ToCanonicalForm2" -> 1.5199949|>, "SameQ" -> False, <<1>>, 
            "Can...entQ" -> <<4>>, "EquivalentQ" -> True|>
```

---

View the sequence of transformations used to convert an expression to its canonical form:

```Mathematica
In[1]:= MakeCanonicalForm[Array[RandomInteger, 5], "Trace" -> True] // Column

Out[1]= ...
```

---

Convert a canonical representation to a normal expression:

```Mathematica
In[1]:= MakeCanonicalForm[Array[RandomInteger, 5]]

Out[1]= Table[R[DiscreteUniformDistribution[{0, S1::Int}]], {S1::Int, 1, 5, 1}]
```

```Mathematica
In[2]:= FromCanonicalForm[%]

Out[2]= Table[RandomVariate[DiscreteUniformDistribution[{0, S1}]], {S1, 1, 5, 1}]
```

Evaluate:

```Mathematica
In[3]:= ReleaseHold[%]

Out[3]= {1, 1, 1, 0, 1}
```

### Neat Examples (6)

Here is a list of expressions, some of which are equivalent to others:

```Mathematica
In[1]:= expressions = {
   HoldForm[Table[i, {i, 5}, {j, i + 2}]],
   HoldForm[Array[Range, 5, 3]],
   HoldForm[Table[ConstantArray[i, i + 2], {i, 5}]],
   HoldForm[First[Rest[Range /@ Range[10]]]],
   HoldForm[Range /@ Range[3, 7]],
   HoldForm[Part[Table[Table[j, {j, i}], {i, 10}], 2]]
   };
```

Find the sequence of transformations for each expression:

```Mathematica
In[2]:= Short[traces = Most@ToCanonicalForm[#, "Trace" -> True] & /@ expressions]

Out[2]//Short= ...
```

Generate a graph for each sequence:

```Mathematica
In[3]:= paths = Graph[DirectedEdge @@@ Partition[#, 2, 1]] & /@ traces

Out[3]= ...
```

Combine the graphs:

```Mathematica
In[4]:= graph = Graph[GraphUnion @@ paths, Sequence[
   VertexLabels -> Placed["Name", Tooltip], 
    GraphLayout -> "LayeredDigraphEmbedding"]];
```

Equivalent expressions converge to the same connected component:

```Mathematica
In[5]:= HighlightGraph[graph, paths]

Out[5]= ... 
```

Group the expressions into their corresponding equivalence class:

```Mathematica
In[6]:= grouped = GroupBy[expressions, ToCanonicalForm]

Out[6]= ...
```

```Mathematica
In[7]:= TableForm[KeyValueMap[Reverse@*List, grouped]]

Out[7]= ...
```