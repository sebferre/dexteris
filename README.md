<meta charset="UTF-8"/>

# What is Dexteris?

Dexteris is a low-code tool for data exploration and transformation. It works as an interactive data-oriented query builder with [JSONiq](https://www.jsoniq.org/) as the target query language. It uses JSON as the pivot data format but it can read from and write to a few other formats: text, CSV, and RDF/Turtle (to be extended to other formats).

Dexteris is very expressive as JSONiq is Turing-complete, and supports a varied set of data processing features:
- reading JSON files, and CSV as JSON (one object per row, one field per column);
- string processing (split, replace, match, ...);
- arithmetics, comparison, and logics;
- accessing and creating JSON data structures;
- iterations, grouping, filtering, aggregates and ordering (FLWOR operators);
- local function definitions.
The built JSONiq programs are high-level, declarative, and concise. Under-progress results are given at every step so that users can keep focused on their data and on the transformations they want to apply.

# Where can I use Dexteris?

Dexteris is freely available as a [client-side web application](http://www.irisa.fr/LIS/ferre/dexteris/), i.e. it runs entirely in your browser. This means that your data remains safely on your machine and is never communicated to any third-party.

A screencast of an example transformation from JSON to CSV is available as a [YouTube video](https://youtu.be/7pzFFaJlu2k).

# Credits

Author: [Sébastien Ferré](http://people.irisa.fr/Sebastien.Ferre/)

Affiliation: Univ. Rennes, team [LACODAM](https://team.inria.fr/lacodam/fr/) at IRISA

Copyright © 2023 Sébastien Ferré, IRISA, Université de Rennes, France

Licence: Apache Licence 2.0

Citation: _Ferré, Sébastien. ‘Dexteris: Data Exploration and Transformation
with a Guided Query Builder Approach’. Int. Conf. Database and Expert Systems Applications (DEXA). LNCS, 2023._
