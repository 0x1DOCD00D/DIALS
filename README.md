# DIstributed Agents Language for Simulations (DIALS)
ddd
## Low-Code Language For Creating Actor-based Model Simulations
Low-code is a popular software engineering approach to enable layman software users to create software applications from pre-built software components with minimal coding. Low-code platforms are widely used in software projects, with four in 10 businesses now using low-code for mission-critical solutions in their business operations, and it is estimated that by 2025, 70% of enterprise applications will be built with low-code technologies. OutSystems and Microsoft's Power Apps  are two examples of the sophisticated low-code development platforms widely used in industry to create software applications without hiring experienced and expensive professional software engineers.

Consider Power Fx, a low-code, general-purpose programming language created by Microsoft. A fragment of the Power Fx application code is shown in Figure~\ref{fig:powerfx} \cite{fx-appcode:2022}. 
```sh
Set(Students, Table( { Name: "Nick", Age: 21 }, { Name: "Sam", Age: 19 }, { Name: "Emma", Age: 25 } ) )
Filter( Students, Age > 20 ) //the result of this operation is {Name:"Nick", Age:21}, {Name:"Emma", Age:25}
```

Using the instruction key \texttt{Set} the table \texttt{Students} key/value pairs are created with the key \texttt{Name} and the value \texttt{Age}. Next, the key \texttt{Filter} designates the operation of projecting this set \texttt{Students} using the criteria that the age of a student should be greater than \texttt{20}. The result is \texttt{\{Name:"Nick", Age:21\}, \{Name:"Emma", Age:25\}}.

We combine the low-code abstraction with the concept of \textit{managed data} that is a two-level approach to data abstraction in which programmers first define data description and manipulation mechanisms, and then use these mechanisms to create a design of an application as a low-code program \cite{DBLP:conf/oopsla/LohSC12,DBLP:journals/scp/StormCL14,DBLP:conf/gpce/ZacharopoulosIS16}. That is, layman software users can specify the organization of an Akka application as a graph where nodes represent actors and edges represent message communications between these actors. The graph specification is created using an open format data interchange key/value pairs language such as Yaml \cite{yaml:2022}. This specification is then processed using our approach that we describe in Section~\ref{sec:implement} to generate actors at runtime with computational payloads ready to respond to clients' requests.

