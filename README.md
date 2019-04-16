# data-sculptor
Artificial data set generator for machine learning purpose

Source: Borland Delphi 6
Release: Microsoft Windows 7  x64

You can define data-sets with three attribute types:
- Real valued
- Integer valued 
- Symbolic

Numeric attribute types can be either normal or uniform distribution.
You can define dependence relations between attributes as well as the importance degree or weight of each relation.
A level of noise can also be define for each attribute.

The generated data set can be exported as an ARFF file formart to be used from the Weka machine learning tool. 

A number of metrics are calculated over the generated data-set. These set of metric can be used as a characterization of the data-set for meta-learning purposes.
