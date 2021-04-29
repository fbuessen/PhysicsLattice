(* ::Package:: *)

(* ::Title:: *)
(*Physics lattices package*)


BeginPackage["PhysicsLattice`"];


(* ::Chapter:: *)
(*Interface*)


(* ::Subsection:: *)
(*Symbols*)


(* ::Input::Initialization:: *)
Protect[LatticeConstant,Type,FirstBZ,ExtendedBZ,LatticeSize,LatticeRange];


(* ::Subsection:: *)
(*Functions*)


(* ::Input::Initialization:: *)
GetLattice::usage="GetLattice[LatticeIdentifier] returns information about the specified lattice.
LatticeIdentifier is the String-form name of the lattice, e.g. one of {Square, Triangular, Honeycomb, Kagome, Cubic, Diamond, Fcc, Hyperkagome, Pyrochlore, Hyperhoneycomb, Hyperoctagon}.

Possible options:
LatticeConstant->Float (default: 1.0. Defined as nearest-neighbor distance)";

GetBrillouinZone::usage="GetBrillouinZone[LatticeIdentifier] returns the Brillouin zone parametrized as a BoundaryMeshRegion. 
LatticeIdentifier the String-form name of the lattice, see GetLattice[].

Possible options:
Type->[FirstBZ|ExtendedBZ] (default: FirstBZ)
Options of GetLattice[]";

GetBrillouinZoneDiscretization::usage="GetBrillouinZoneDiscretization[LatticeIdentifier] returns a discretization of the given two-dimensional BZ.
LatticeIdentifier the String-form name of the lattice, see GetLattice[].

Possible options:
LatticeSize->Int (default: 16. Number of unit cells in all directions assumed for periodic boundary conditions)
ClippingStyle->[Automatic|None] (default:Automatic. Choosing Automatic clips the parametrization to the Brillouin zone. Choosing None clips the parametrization to a rectangular bounding box around the Brillouin zone.)
MaxRecursion->Int (default: 4. Maximum number of recursions to build up the discretization)
Options of GetBrillouinZone[]";

GetBrillouinZoneDiscretization3D::usage="GetBrillouinZoneDiscretization3D[LatticeIdentifier] returns a discretization of the given three-dimensional BZ.
LatticeIdentifier the String-form name of the lattice, see GetLattice[].

Possible options:
LatticeSize->Int (default: 16. Number of unit cells in all directions assumed for periodic boundary conditions)
ClippingStyle->[Automatic|None] (default:Automatic. Choosing Automatic clips the parametrization to the Brillouin zone. Choosing None clips the parametrization to a rectangular bounding box around the Brillouin zone.)
MaxRecursion->Int (default: 4. Maximum number of recursions to build up the discretization)
Options of GetBrillouinZone[]";

PlotLattice::usage="PlotLattice[LatticeIdentifier] plots a given two-dimensional lattice.
LatticeIdentifier can either be the String-form name of the lattice, see GetLattice[], or a list {Directive[],LatticeIdentifier} of directives and the String-form name of the lattice.

Possible options:
LatticeRange->Float (default:4. Maximum distance of drawn sites from the origin)
MaxRecursion->Int (default: 4. Maximum number of recursions to build up the discretization)
Options of GetLattice[]
Options of Graphics[]";

PlotLattice3D::usage="PlotLattice3D[LatticeIdentifier] plots a given two-dimensional lattice.
LatticeIdentifier can either be the String-form name of the lattice, see GetLattice[], or a list {Directive[],LatticeIdentifier} of directives and the String-form name of the lattice.

Possible options:
LatticeRange->Float (default:4. Maximum distance of drawn sites from the origin)
MaxRecursion->Int (default: 4. Maximum number of recursions to build up the discretization)
Options of GetLattice[]
Options of Graphics3D[]";

PlotBrillouinZone::usage="PlotBrillouinZone[LatticeIdentifier] plots the Brillouin zone of a given two-dimensional lattice.
LatticeIdentifier can either be the String-form name of the lattice, see GetLattice[], or a list {Directive[],LatticeIdentifier} of directives and the String-form name of the lattice.

Possible options:
Options of GetBrillouinZone[]
Options of Graphics[]";

PlotBrillouinZone3D::usage="PlotBrillouinZone3D[LatticeIdentifier] plots the Brillouin zone of a given three-dimensional lattice.
LatticeIdentifier can either be the String-form name of the lattice, see GetLattice[], or a list {Directive[],LatticeIdentifier} of directives and the String-form name of the lattice.

Possible options:
Options of GetBrillouinZone[]
Options of Graphics3D[]";


(* ::Chapter:: *)
(*Implementation*)


Begin["`Private`"];


(* ::Section:: *)
(*Data*)


(* ::Input::Initialization:: *)
data=<|
"Square"-> <|
"Basis"->{{0,0}},
"LatticeVectors"->{{1,0},{0,1}},
"ReciprocalLatticeVectors"->{{2\[Pi],0},{0,2\[Pi]}},
"BrillouinZone"->BoundaryMeshRegion[{{\[Pi],\[Pi]},{-\[Pi],\[Pi]},{-\[Pi],-\[Pi]},{\[Pi],-\[Pi]}},Line[{1,2,3,4,1}]]
|>,
"Triangular"-><|
"Basis"->{{0,0}},
"LatticeVectors"->{{Sqrt[3]/2,1/2},{Sqrt[3]/2,-1/2}},
"ReciprocalLatticeVectors"->{{2 \[Pi]/Sqrt[3],2 \[Pi]},{2 \[Pi]/Sqrt[3],-2 \[Pi]}},
"BrillouinZone"->BoundaryMeshRegion[{{0,-4\[Pi]/3},{2\[Pi]/Sqrt[3],-2\[Pi]/3},{2\[Pi]/Sqrt[3],2\[Pi]/3},{0,4\[Pi]/3},{-2\[Pi]/Sqrt[3],2\[Pi]/3},{-2\[Pi]/Sqrt[3],-2\[Pi]/3}},Line[{1,2,3,4,5,6,1}]]
|>,
"Honeycomb"-><|
"Basis"->{{0,0},{1,0}},
"LatticeVectors"->{{3/2,Sqrt[3]/2},{3/2,-Sqrt[3]/2}},
"ReciprocalLatticeVectors"->{{2 \[Pi]/3,2 \[Pi]/Sqrt[3]},{2 \[Pi]/3,-2 \[Pi]/Sqrt[3]}},
"BrillouinZone"->BoundaryMeshRegion[{{0,-4\[Pi]/(3Sqrt[3])},{2\[Pi]/3,-2\[Pi]/(3Sqrt[3])},{2\[Pi]/3,2\[Pi]/(3Sqrt[3])},{0,4\[Pi]/(3Sqrt[3])},{-2\[Pi]/3,2\[Pi]/(3Sqrt[3])},{-2\[Pi]/3,-2\[Pi]/(3Sqrt[3])}},Line[{1,2,3,4,5,6,1}]],
"ExtendedBrillouinZone"->BoundaryMeshRegion[{{4\[Pi]/3,0},{2\[Pi]/3,2\[Pi]/Sqrt[3]},{-2\[Pi]/3,2\[Pi]/Sqrt[3]},{-4\[Pi]/3,0},{-2\[Pi]/3,-2\[Pi]/Sqrt[3]},{2\[Pi]/3,-2\[Pi]/Sqrt[3]}},Line[{1,2,3,4,5,6,1}]] 
|>,
"Kagome"-><|
"Basis"->{{0,0},{1,0},{1/2,Sqrt[3]/2}},
"LatticeVectors"->{{2,0},{1,Sqrt[3]}},
"ReciprocalLatticeVectors"->{{\[Pi],-\[Pi]/Sqrt[3]},{0,2 \[Pi]/Sqrt[3]}},
"BrillouinZone"->BoundaryMeshRegion[{{2\[Pi]/3,0},{\[Pi]/3,\[Pi]/Sqrt[3]},{-\[Pi]/3,\[Pi]/Sqrt[3]},{-2\[Pi]/3,0},{-\[Pi]/3,-\[Pi]/Sqrt[3]},{\[Pi]/3,-\[Pi]/Sqrt[3]},{2\[Pi]/3,0}},Line[{1,2,3,4,5,6,1}]],
"ExtendedBrillouinZone"-> BoundaryMeshRegion[{{4\[Pi]/3,0},{2\[Pi]/3,2\[Pi]/Sqrt[3]},{-2\[Pi]/3,2\[Pi]/Sqrt[3]},{-4\[Pi]/3,0},{-2\[Pi]/3,-2\[Pi]/Sqrt[3]},{2\[Pi]/3,-2\[Pi]/Sqrt[3]},{4\[Pi]/3,0}},Line[{1,2,3,4,5,6,1}]]
|>,
"Cubic"-><|
"Basis"->{{0,0,0}},
"LatticeVectors"->{{1,0,0},{0,1,0},{0,0,1}},
"ReciprocalLatticeVectors"->{{2 \[Pi],0,0},{0,2 \[Pi],0},{0,0,2 \[Pi]}},
"BrillouinZone"->BoundaryMeshRegion[{{\[Pi],\[Pi],\[Pi]},{-\[Pi],\[Pi],\[Pi]},{-\[Pi],-\[Pi],\[Pi]},{\[Pi],-\[Pi],\[Pi]},{\[Pi],\[Pi],-\[Pi]},{-\[Pi],\[Pi],-\[Pi]},{-\[Pi],-\[Pi],-\[Pi]},{\[Pi],-\[Pi],-\[Pi]}},Polygon[{{1,2,3,4},{1,5,6,2},{2,6,7,3},{3,7,8,4},{4,8,5,1},{8,7,6,5}}]]
|>,
"Diamond"-><|
"Basis"->{{0,0,0},{1/Sqrt[3],1/Sqrt[3],1/Sqrt[3]}},
"LatticeVectors"->{{0,2/Sqrt[3],2/Sqrt[3]},{2/Sqrt[3],0,2/Sqrt[3]},{2/Sqrt[3],2/Sqrt[3],0}},
"ReciprocalLatticeVectors"->{{-Sqrt[3] \[Pi]/2,Sqrt[3] \[Pi]/2,Sqrt[3] \[Pi]/2},{Sqrt[3] \[Pi]/2,-Sqrt[3] \[Pi]/2,Sqrt[3] \[Pi]/2},{Sqrt[3] \[Pi]/2,Sqrt[3] \[Pi]/2,-Sqrt[3] \[Pi]/2}},
"BrillouinZone"->BoundaryMeshRegion[{{(Sqrt[3] \[Pi])/4,0,(Sqrt[3] \[Pi])/2},{0,(Sqrt[3] \[Pi])/4,(Sqrt[3] \[Pi])/2},{-((Sqrt[3] \[Pi])/4),0,(Sqrt[3] \[Pi])/2},{0,-((Sqrt[3] \[Pi])/4),(Sqrt[3] \[Pi])/2},{(Sqrt[3] \[Pi])/2,0,(Sqrt[3] \[Pi])/4},{(Sqrt[3] \[Pi])/2,-((Sqrt[3] \[Pi])/4),0},{(Sqrt[3] \[Pi])/2,0,-((Sqrt[3] \[Pi])/4)},{(Sqrt[3] \[Pi])/2,(Sqrt[3] \[Pi])/4,0},{0,(Sqrt[3] \[Pi])/2,(Sqrt[3] \[Pi])/4},{(Sqrt[3] \[Pi])/4,(Sqrt[3] \[Pi])/2,0},{0,(Sqrt[3] \[Pi])/2,-((Sqrt[3] \[Pi])/4)},{-((Sqrt[3] \[Pi])/4),(Sqrt[3] \[Pi])/2,0},{-((Sqrt[3] \[Pi])/2),0,(Sqrt[3] \[Pi])/4},{-((Sqrt[3] \[Pi])/2),(Sqrt[3] \[Pi])/4,0},{-((Sqrt[3] \[Pi])/2),0,-((Sqrt[3] \[Pi])/4)},{-((Sqrt[3] \[Pi])/2),-((Sqrt[3] \[Pi])/4),0},{0,-((Sqrt[3] \[Pi])/2),(Sqrt[3] \[Pi])/4},{-((Sqrt[3] \[Pi])/4),-((Sqrt[3] \[Pi])/2),0},{0,-((Sqrt[3] \[Pi])/2),-((Sqrt[3] \[Pi])/4)},{(Sqrt[3] \[Pi])/4,-((Sqrt[3] \[Pi])/2),0},{(Sqrt[3] \[Pi])/4,0,-((Sqrt[3] \[Pi])/2)},{0,-((Sqrt[3] \[Pi])/4),-((Sqrt[3] \[Pi])/2)},{-((Sqrt[3] \[Pi])/4),0,-((Sqrt[3] \[Pi])/2)},{0,(Sqrt[3] \[Pi])/4,-((Sqrt[3] \[Pi])/2)}},Polygon[{{1,2,3,4},{5,6,7,8},{9,10,11,12},{13,14,15,16},{17,18,19,20},{21,22,23,24},{1,2,9,10,8,5},{2,3,13,14,12,9},{3,4,17,18,16,13},{4,1,5,6,20,17},{21,24,11,10,8,7},{24,23,15,14,12,11},{23,22,19,18,16,15},{22,21,7,6,20,19}}]],
"ExtendedBrillouinZone"-> BoundaryMeshRegion[{{0,0,Sqrt[3] \[Pi]},{(Sqrt[3] \[Pi])/2,(Sqrt[3] \[Pi])/2,(Sqrt[3] \[Pi])/2},{Sqrt[3] \[Pi],0,0},{(Sqrt[3] \[Pi])/2,-((Sqrt[3] \[Pi])/2),(Sqrt[3] \[Pi])/2},{0,Sqrt[3] \[Pi],0},{-((Sqrt[3] \[Pi])/2),(Sqrt[3] \[Pi])/2,(Sqrt[3] \[Pi])/2},{-Sqrt[3] \[Pi],0,0},{-((Sqrt[3] \[Pi])/2),-((Sqrt[3] \[Pi])/2),(Sqrt[3] \[Pi])/2},{0,-Sqrt[3] \[Pi],0},{0,0,-Sqrt[3] \[Pi]},{(Sqrt[3] \[Pi])/2,(Sqrt[3] \[Pi])/2,-((Sqrt[3] \[Pi])/2)},{(Sqrt[3] \[Pi])/2,-((Sqrt[3] \[Pi])/2),-((Sqrt[3] \[Pi])/2)},{-((Sqrt[3] \[Pi])/2),(Sqrt[3] \[Pi])/2,-((Sqrt[3] \[Pi])/2)},{-((Sqrt[3] \[Pi])/2),-((Sqrt[3] \[Pi])/2),-((Sqrt[3] \[Pi])/2)}},Polygon[{{1,8,9,4},{1,4,3,2},{1,2,5,6},{1,6,7,8},{4,9,12,3},{2,3,11,5},{6,5,13,7},{8,7,14,9},{3,12,10,11},{5,11,10,13},{7,13,10,14},{9,14,10,12}}]]
|>,
"Fcc"-><|
"Basis"->{{0,0,0}},
"LatticeVectors"->{{0,1/Sqrt[2],1/Sqrt[2]},{1/Sqrt[2],0,1/Sqrt[2]},{1/Sqrt[2],1/Sqrt[2],0}},
"ReciprocalLatticeVectors"->{{-Sqrt[2] \[Pi],Sqrt[2] \[Pi],Sqrt[2] \[Pi]},{Sqrt[2] \[Pi],-Sqrt[2] \[Pi],Sqrt[2] \[Pi]},{Sqrt[2] \[Pi],Sqrt[2] \[Pi],-Sqrt[2] \[Pi]}},
"BrillouinZone"->BoundaryMeshRegion[{{\[Pi]/Sqrt[2],0,Sqrt[2] \[Pi]},{0,\[Pi]/Sqrt[2],Sqrt[2] \[Pi]},{-(\[Pi]/Sqrt[2]),0,Sqrt[2] \[Pi]},{0,-(\[Pi]/Sqrt[2]),Sqrt[2] \[Pi]},{Sqrt[2] \[Pi],0,\[Pi]/Sqrt[2]},{Sqrt[2] \[Pi],-(\[Pi]/Sqrt[2]),0},{Sqrt[2] \[Pi],0,-(\[Pi]/Sqrt[2])},{Sqrt[2] \[Pi],\[Pi]/Sqrt[2],0},{0,Sqrt[2] \[Pi],\[Pi]/Sqrt[2]},{\[Pi]/Sqrt[2],Sqrt[2] \[Pi],0},{0,Sqrt[2] \[Pi],-(\[Pi]/Sqrt[2])},{-(\[Pi]/Sqrt[2]),Sqrt[2] \[Pi],0},{-Sqrt[2] \[Pi],0,\[Pi]/Sqrt[2]},{-Sqrt[2] \[Pi],\[Pi]/Sqrt[2],0},{-Sqrt[2] \[Pi],0,-(\[Pi]/Sqrt[2])},{-Sqrt[2] \[Pi],-(\[Pi]/Sqrt[2]),0},{0,-Sqrt[2] \[Pi],\[Pi]/Sqrt[2]},{-(\[Pi]/Sqrt[2]),-Sqrt[2] \[Pi],0},{0,-Sqrt[2] \[Pi],-(\[Pi]/Sqrt[2])},{\[Pi]/Sqrt[2],-Sqrt[2] \[Pi],0},{\[Pi]/Sqrt[2],0,-Sqrt[2] \[Pi]},{0,-(\[Pi]/Sqrt[2]),-Sqrt[2] \[Pi]},{-(\[Pi]/Sqrt[2]),0,-Sqrt[2] \[Pi]},{0,\[Pi]/Sqrt[2],-Sqrt[2] \[Pi]}},Polygon[{{1,2,3,4},{5,6,7,8},{9,10,11,12},{13,14,15,16},{17,18,19,20},{21,22,23,24},{1,2,9,10,8,5},{2,3,13,14,12,9},{3,4,17,18,16,13},{4,1,5,6,20,17},{21,24,11,10,8,7},{24,23,15,14,12,11},{23,22,19,18,16,15},{22,21,7,6,20,19}}]] 
|>,
"Hyperkagome"-><|
"Basis"->{{7/2/Sqrt[2],1/2/Sqrt[2],1/2/Sqrt[2]},{5/2/Sqrt[2],3/2/Sqrt[2],1/2/Sqrt[2]},{7/2/Sqrt[2],3/2/Sqrt[2],3/2/Sqrt[2]},{7/2/Sqrt[2],5/2/Sqrt[2],5/2/Sqrt[2]},{5/2/Sqrt[2],5/2/Sqrt[2],7/2/Sqrt[2]},{3/2/Sqrt[2],3/2/Sqrt[2],7/2/Sqrt[2]},{3/2/Sqrt[2],1/2/Sqrt[2],5/2/Sqrt[2]},{1/2/Sqrt[2],1/2/Sqrt[2],7/2/Sqrt[2]},{5/2/Sqrt[2],7/2/Sqrt[2],5/2/Sqrt[2]},{3/2/Sqrt[2],7/2/Sqrt[2],3/2/Sqrt[2]},{1/2/Sqrt[2],7/2/Sqrt[2],1/2/Sqrt[2]},{1/2/Sqrt[2],5/2/Sqrt[2],3/2/Sqrt[2]}},
"LatticeVectors"->{{2Sqrt[2],0,0},{0,2Sqrt[2],0},{0,0,2Sqrt[2]}},
"ReciprocalLatticeVectors"->{{\[Pi]/Sqrt[2],0,0},{0,\[Pi]/Sqrt[2],0},{0,0,\[Pi]/Sqrt[2]}},
"BrillouinZone"->BoundaryMeshRegion[{{\[Pi]/(2 Sqrt[2]),\[Pi]/(2 Sqrt[2]),\[Pi]/(2 Sqrt[2])},{-(\[Pi]/(2 Sqrt[2])),\[Pi]/(2 Sqrt[2]),\[Pi]/(2 Sqrt[2])},{-(\[Pi]/(2 Sqrt[2])),-(\[Pi]/(2 Sqrt[2])),\[Pi]/(2 Sqrt[2])},{\[Pi]/(2 Sqrt[2]),-(\[Pi]/(2 Sqrt[2])),\[Pi]/(2 Sqrt[2])},{\[Pi]/(2 Sqrt[2]),\[Pi]/(2 Sqrt[2]),-(\[Pi]/(2 Sqrt[2]))},{-(\[Pi]/(2 Sqrt[2])),\[Pi]/(2 Sqrt[2]),-(\[Pi]/(2 Sqrt[2]))},{-(\[Pi]/(2 Sqrt[2])),-(\[Pi]/(2 Sqrt[2])),-(\[Pi]/(2 Sqrt[2]))},{\[Pi]/(2 Sqrt[2]),-(\[Pi]/(2 Sqrt[2])),-(\[Pi]/(2 Sqrt[2]))}},Polygon[{{1,2,3,4},{1,5,6,2},{2,6,7,3},{4,3,7,8},{1,4,8,5},{5,8,7,6}}]],
"ExtendedBrillouinZone"-> BoundaryMeshRegion[{{\[Pi]/Sqrt[2],0,Sqrt[2] \[Pi]},{0,\[Pi]/Sqrt[2],Sqrt[2] \[Pi]},{-(\[Pi]/Sqrt[2]),0,Sqrt[2] \[Pi]},{0,-(\[Pi]/Sqrt[2]),Sqrt[2] \[Pi]},{Sqrt[2] \[Pi],0,\[Pi]/Sqrt[2]},{Sqrt[2] \[Pi],-(\[Pi]/Sqrt[2]),0},{Sqrt[2] \[Pi],0,-(\[Pi]/Sqrt[2])},{Sqrt[2] \[Pi],\[Pi]/Sqrt[2],0},{0,Sqrt[2] \[Pi],\[Pi]/Sqrt[2]},{\[Pi]/Sqrt[2],Sqrt[2] \[Pi],0},{0,Sqrt[2] \[Pi],-(\[Pi]/Sqrt[2])},{-(\[Pi]/Sqrt[2]),Sqrt[2] \[Pi],0},{-Sqrt[2] \[Pi],0,\[Pi]/Sqrt[2]},{-Sqrt[2] \[Pi],\[Pi]/Sqrt[2],0},{-Sqrt[2] \[Pi],0,-(\[Pi]/Sqrt[2])},{-Sqrt[2] \[Pi],-(\[Pi]/Sqrt[2]),0},{0,-Sqrt[2] \[Pi],\[Pi]/Sqrt[2]},{-(\[Pi]/Sqrt[2]),-Sqrt[2] \[Pi],0},{0,-Sqrt[2] \[Pi],-(\[Pi]/Sqrt[2])},{\[Pi]/Sqrt[2],-Sqrt[2] \[Pi],0},{\[Pi]/Sqrt[2],0,-Sqrt[2] \[Pi]},{0,-(\[Pi]/Sqrt[2]),-Sqrt[2] \[Pi]},{-(\[Pi]/Sqrt[2]),0,-Sqrt[2] \[Pi]},{0,\[Pi]/Sqrt[2],-Sqrt[2] \[Pi]}},Polygon[{{1,2,3,4},{5,6,7,8},{9,10,11,12},{13,14,15,16},{17,18,19,20},{21,22,23,24},{1,2,9,10,8,5},{2,3,13,14,12,9},{3,4,17,18,16,13},{4,1,5,6,20,17},{21,24,11,10,8,7},{24,23,15,14,12,11},{23,22,19,18,16,15},{22,21,7,6,20,19}}]]
|>,
"Pyrochlore"-><|
"Basis"->{{0,0,0},{0, 1/Sqrt[2], 1/Sqrt[2]},{ 1/Sqrt[2],0, 1/Sqrt[2]},{ 1/Sqrt[2], 1/Sqrt[2],0}},
"LatticeVectors"->{{0,2/Sqrt[2],2/Sqrt[2]},{2/Sqrt[2],0,2/Sqrt[2]},{2/Sqrt[2],2/Sqrt[2],0}},
"ReciprocalLatticeVectors"->{{-\[Pi]/Sqrt[2],\[Pi]/Sqrt[2],\[Pi]/Sqrt[2]},{\[Pi]/Sqrt[2],-\[Pi]/Sqrt[2],\[Pi]/Sqrt[2]},{\[Pi]/Sqrt[2],\[Pi]/Sqrt[2],-\[Pi]/Sqrt[2]}},
"BrillouinZone"->BoundaryMeshRegion[{{\[Pi]/(2 Sqrt[2]),0,\[Pi]/Sqrt[2]},{0,\[Pi]/(2 Sqrt[2]),\[Pi]/Sqrt[2]},{-(\[Pi]/(2 Sqrt[2])),0,\[Pi]/Sqrt[2]},{0,-(\[Pi]/(2 Sqrt[2])),\[Pi]/Sqrt[2]},{\[Pi]/Sqrt[2],0,\[Pi]/(2 Sqrt[2])},{\[Pi]/Sqrt[2],-(\[Pi]/(2 Sqrt[2])),0},{\[Pi]/Sqrt[2],0,-(\[Pi]/(2 Sqrt[2]))},{\[Pi]/Sqrt[2],\[Pi]/(2 Sqrt[2]),0},{0,\[Pi]/Sqrt[2],\[Pi]/(2 Sqrt[2])},{\[Pi]/(2 Sqrt[2]),\[Pi]/Sqrt[2],0},{0,\[Pi]/Sqrt[2],-(\[Pi]/(2 Sqrt[2]))},{-(\[Pi]/(2 Sqrt[2])),\[Pi]/Sqrt[2],0},{-(\[Pi]/Sqrt[2]),0,\[Pi]/(2 Sqrt[2])},{-(\[Pi]/Sqrt[2]),\[Pi]/(2 Sqrt[2]),0},{-(\[Pi]/Sqrt[2]),0,-(\[Pi]/(2 Sqrt[2]))},{-(\[Pi]/Sqrt[2]),-(\[Pi]/(2 Sqrt[2])),0},{0,-(\[Pi]/Sqrt[2]),\[Pi]/(2 Sqrt[2])},{-(\[Pi]/(2 Sqrt[2])),-(\[Pi]/Sqrt[2]),0},{0,-(\[Pi]/Sqrt[2]),-(\[Pi]/(2 Sqrt[2]))},{\[Pi]/(2 Sqrt[2]),-(\[Pi]/Sqrt[2]),0},{\[Pi]/(2 Sqrt[2]),0,-(\[Pi]/Sqrt[2])},{0,-(\[Pi]/(2 Sqrt[2])),-(\[Pi]/Sqrt[2])},{-(\[Pi]/(2 Sqrt[2])),0,-(\[Pi]/Sqrt[2])},{0,\[Pi]/(2 Sqrt[2]),-(\[Pi]/Sqrt[2])}},Polygon[{{1,2,3,4},{5,6,7,8},{9,10,11,12},{13,14,15,16},{17,18,19,20},{21,22,23,24},{1,2,9,10,8,5},{2,3,13,14,12,9},{3,4,17,18,16,13},{4,1,5,6,20,17},{21,24,11,10,8,7},{24,23,15,14,12,11},{23,22,19,18,16,15},{22,21,7,6,20,19}}]],
"ExtendedBrillouinZone"-> BoundaryMeshRegion[{{\[Pi]/Sqrt[2],0,Sqrt[2] \[Pi]},{0,\[Pi]/Sqrt[2],Sqrt[2] \[Pi]},{-(\[Pi]/Sqrt[2]),0,Sqrt[2] \[Pi]},{0,-(\[Pi]/Sqrt[2]),Sqrt[2] \[Pi]},{Sqrt[2] \[Pi],0,\[Pi]/Sqrt[2]},{Sqrt[2] \[Pi],-(\[Pi]/Sqrt[2]),0},{Sqrt[2] \[Pi],0,-(\[Pi]/Sqrt[2])},{Sqrt[2] \[Pi],\[Pi]/Sqrt[2],0},{0,Sqrt[2] \[Pi],\[Pi]/Sqrt[2]},{\[Pi]/Sqrt[2],Sqrt[2] \[Pi],0},{0,Sqrt[2] \[Pi],-(\[Pi]/Sqrt[2])},{-(\[Pi]/Sqrt[2]),Sqrt[2] \[Pi],0},{-Sqrt[2] \[Pi],0,\[Pi]/Sqrt[2]},{-Sqrt[2] \[Pi],\[Pi]/Sqrt[2],0},{-Sqrt[2] \[Pi],0,-(\[Pi]/Sqrt[2])},{-Sqrt[2] \[Pi],-(\[Pi]/Sqrt[2]),0},{0,-Sqrt[2] \[Pi],\[Pi]/Sqrt[2]},{-(\[Pi]/Sqrt[2]),-Sqrt[2] \[Pi],0},{0,-Sqrt[2] \[Pi],-(\[Pi]/Sqrt[2])},{\[Pi]/Sqrt[2],-Sqrt[2] \[Pi],0},{\[Pi]/Sqrt[2],0,-Sqrt[2] \[Pi]},{0,-(\[Pi]/Sqrt[2]),-Sqrt[2] \[Pi]},{-(\[Pi]/Sqrt[2]),0,-Sqrt[2] \[Pi]},{0,\[Pi]/Sqrt[2],-Sqrt[2] \[Pi]}},Polygon[{{1,2,3,4},{5,6,7,8},{9,10,11,12},{13,14,15,16},{17,18,19,20},{21,22,23,24},{1,2,9,10,8,5},{2,3,13,14,12,9},{3,4,17,18,16,13},{4,1,5,6,20,17},{21,24,11,10,8,7},{24,23,15,14,12,11},{23,22,19,18,16,15},{22,21,7,6,20,19}}]]
|>,
"Hyperhoneycomb"-><|
"Basis"->{{0,0,0},{1/Sqrt[2],1/Sqrt[2],0},{1/Sqrt[2],2/Sqrt[2],1/Sqrt[2]},{0,-1/Sqrt[2],1/Sqrt[2]}},
"LatticeVectors"->{{-1/Sqrt[2],1/Sqrt[2],-2/Sqrt[2]},{-1/Sqrt[2],1/Sqrt[2],2/Sqrt[2]},{2/Sqrt[2],4/Sqrt[2],0}},
"ReciprocalLatticeVectors"->{{-2 Sqrt[2] \[Pi]/3,Sqrt[2] \[Pi]/3,-\[Pi]/Sqrt[2]},{-2 Sqrt[2] \[Pi]/3,Sqrt[2] \[Pi]/3,\[Pi]/Sqrt[2]},{Sqrt[2] \[Pi]/3,Sqrt[2] \[Pi]/3,0}},
"BrillouinZone"->BoundaryMeshRegion[{{(11 \[Pi])/(36 Sqrt[2]),-((11 \[Pi])/(36 Sqrt[2])),\[Pi]/Sqrt[2]},{(29 \[Pi])/(36 Sqrt[2]),-((29 \[Pi])/(36 Sqrt[2])),0},{(37 \[Pi])/(36 Sqrt[2]),-((13 \[Pi])/(36 Sqrt[2])),0},{(19 \[Pi])/(36 Sqrt[2]),(5 \[Pi])/(36 Sqrt[2]),\[Pi]/Sqrt[2]},{(11 \[Pi])/(36 Sqrt[2]),-((11 \[Pi])/(36 Sqrt[2])),-(\[Pi]/Sqrt[2])},{(19 \[Pi])/(36 Sqrt[2]),(5 \[Pi])/(36 Sqrt[2]),-(\[Pi]/Sqrt[2])},{-((5 \[Pi])/(36 Sqrt[2])),-((19 \[Pi])/(36 Sqrt[2])),\[Pi]/Sqrt[2]},{-((5 \[Pi])/(36 Sqrt[2])),-((19 \[Pi])/(36 Sqrt[2])),-(\[Pi]/Sqrt[2])},{(13 \[Pi])/(36 Sqrt[2]),-((37 \[Pi])/(36 Sqrt[2])),0},{-((11 \[Pi])/(36 Sqrt[2])),(11 \[Pi])/(36 Sqrt[2]),-(\[Pi]/Sqrt[2])},{-((29 \[Pi])/(36 Sqrt[2])),(29 \[Pi])/(36 Sqrt[2]),0},{-((37 \[Pi])/(36 Sqrt[2])),(13 \[Pi])/(36 Sqrt[2]),0},{-((19 \[Pi])/(36 Sqrt[2])),-((5 \[Pi])/(36 Sqrt[2])),-(\[Pi]/Sqrt[2])},{-((11 \[Pi])/(36 Sqrt[2])),(11 \[Pi])/(36 Sqrt[2]),\[Pi]/Sqrt[2]},{-((19 \[Pi])/(36 Sqrt[2])),-((5 \[Pi])/(36 Sqrt[2])),\[Pi]/Sqrt[2]},{(5 \[Pi])/(36 Sqrt[2]),(19 \[Pi])/(36 Sqrt[2]),-(\[Pi]/Sqrt[2])},{(5 \[Pi])/(36 Sqrt[2]),(19 \[Pi])/(36 Sqrt[2]),\[Pi]/Sqrt[2]},{-((13 \[Pi])/(36 Sqrt[2])),(37 \[Pi])/(36 Sqrt[2]),0}},Polygon[{{1,4,17,14,15,7},{1,2,3,4},{1,7,9,2},{14,11,12,15},{14,17,18,11},{11,10,13,12},{11,18,16,10},{2,9,8,5},{2,5,6,3},{6,5,8,13,10,16},{7,15,12,13,8,9},{4,3,6,16,18,17}}]],
"ExtendedBrillouinZone"-> BoundaryMeshRegion[{{\[Pi]/Sqrt[2],0,Sqrt[2] \[Pi]},{0,\[Pi]/Sqrt[2],Sqrt[2] \[Pi]},{-(\[Pi]/Sqrt[2]),0,Sqrt[2] \[Pi]},{0,-(\[Pi]/Sqrt[2]),Sqrt[2] \[Pi]},{Sqrt[2] \[Pi],0,\[Pi]/Sqrt[2]},{Sqrt[2] \[Pi],-(\[Pi]/Sqrt[2]),0},{Sqrt[2] \[Pi],0,-(\[Pi]/Sqrt[2])},{Sqrt[2] \[Pi],\[Pi]/Sqrt[2],0},{0,Sqrt[2] \[Pi],\[Pi]/Sqrt[2]},{\[Pi]/Sqrt[2],Sqrt[2] \[Pi],0},{0,Sqrt[2] \[Pi],-(\[Pi]/Sqrt[2])},{-(\[Pi]/Sqrt[2]),Sqrt[2] \[Pi],0},{-Sqrt[2] \[Pi],0,\[Pi]/Sqrt[2]},{-Sqrt[2] \[Pi],\[Pi]/Sqrt[2],0},{-Sqrt[2] \[Pi],0,-(\[Pi]/Sqrt[2])},{-Sqrt[2] \[Pi],-(\[Pi]/Sqrt[2]),0},{0,-Sqrt[2] \[Pi],\[Pi]/Sqrt[2]},{-(\[Pi]/Sqrt[2]),-Sqrt[2] \[Pi],0},{0,-Sqrt[2] \[Pi],-(\[Pi]/Sqrt[2])},{\[Pi]/Sqrt[2],-Sqrt[2] \[Pi],0},{\[Pi]/Sqrt[2],0,-Sqrt[2] \[Pi]},{0,-(\[Pi]/Sqrt[2]),-Sqrt[2] \[Pi]},{-(\[Pi]/Sqrt[2]),0,-Sqrt[2] \[Pi]},{0,\[Pi]/Sqrt[2],-Sqrt[2] \[Pi]}},Polygon[{{1,2,3,4},{5,6,7,8},{9,10,11,12},{13,14,15,16},{17,18,19,20},{21,22,23,24},{1,2,9,10,8,5},{2,3,13,14,12,9},{3,4,17,18,16,13},{4,1,5,6,20,17},{21,24,11,10,8,7},{24,23,15,14,12,11},{23,22,19,18,16,15},{22,21,7,6,20,19}}]]
|>,
"Hyperoctagon"-><|
"Basis"->{{1/Sqrt[8],1/Sqrt[8],1/Sqrt[8]},{5/Sqrt[8],3/Sqrt[8],-1/Sqrt[8]},{3/Sqrt[8],1/Sqrt[8],-1/Sqrt[8]},{7/Sqrt[8],3/Sqrt[8],1/Sqrt[8]}},
"LatticeVectors"->{{Sqrt[8],0,0},{Sqrt[8]/2,Sqrt[8]/2,-Sqrt[8]/2},{Sqrt[8]/2,Sqrt[8]/2,Sqrt[8]/2}},
"ReciprocalLatticeVectors"->{{\[Pi]/Sqrt[2],-\[Pi]/Sqrt[2],0},{0,\[Pi]/Sqrt[2],-\[Pi]/Sqrt[2]},{0,\[Pi]/Sqrt[2],\[Pi]/Sqrt[2]}},
"BrillouinZone"->BoundaryMeshRegion[{{\[Pi]/Sqrt[2],0,0},{-\[Pi]/Sqrt[2],0,0},{0,\[Pi]/Sqrt[2],0},{0,-\[Pi]/Sqrt[2],0},{0,0,\[Pi]/Sqrt[2]},{0,0,-\[Pi]/Sqrt[2]},{Sqrt[2]\[Pi]/4,Sqrt[2]\[Pi]/4,Sqrt[2]\[Pi]/4},{-Sqrt[2]\[Pi]/4,Sqrt[2]\[Pi]/4,Sqrt[2]\[Pi]/4},{Sqrt[2]\[Pi]/4,-Sqrt[2]\[Pi]/4,Sqrt[2]\[Pi]/4},{-Sqrt[2]\[Pi]/4,-Sqrt[2]\[Pi]/4,Sqrt[2]\[Pi]/4},{Sqrt[2]\[Pi]/4,Sqrt[2]\[Pi]/4,-Sqrt[2]\[Pi]/4},{-Sqrt[2]\[Pi]/4,Sqrt[2]\[Pi]/4,-Sqrt[2]\[Pi]/4},{Sqrt[2]\[Pi]/4,-Sqrt[2]\[Pi]/4,-Sqrt[2]\[Pi]/4},{-Sqrt[2]\[Pi]/4,-Sqrt[2]\[Pi]/4,-Sqrt[2]\[Pi]/4}},
Polygon[{{1,13,6,11},{1,13,4,9},{1,9,5,7},{1,7,3,11},{4,14,6,13},{6,12,3,11},{3,8,5,7},{5,10,4,9},{2,12,6,14},{2,14,4,10},{2,10,5,8},{2,8,3,12}}]],
"ExtendedBrillouinZone"-> BoundaryMeshRegion[{{0,0,2 Sqrt[2] \[Pi]},{Sqrt[2] \[Pi],Sqrt[2] \[Pi],Sqrt[2] \[Pi]},{2 Sqrt[2] \[Pi],0,0},{Sqrt[2] \[Pi],-Sqrt[2] \[Pi],Sqrt[2] \[Pi]},{0,2 Sqrt[2] \[Pi],0},{-Sqrt[2] \[Pi],Sqrt[2] \[Pi],Sqrt[2] \[Pi]},{-2 Sqrt[2] \[Pi],0,0},{-Sqrt[2] \[Pi],-Sqrt[2] \[Pi],Sqrt[2] \[Pi]},{0,-2 Sqrt[2] \[Pi],0},{0,0,-2 Sqrt[2] \[Pi]},{Sqrt[2] \[Pi],Sqrt[2] \[Pi],-Sqrt[2] \[Pi]},{Sqrt[2] \[Pi],-Sqrt[2] \[Pi],-Sqrt[2] \[Pi]},{-Sqrt[2] \[Pi],Sqrt[2] \[Pi],-Sqrt[2] \[Pi]},{-Sqrt[2] \[Pi],-Sqrt[2] \[Pi],-Sqrt[2] \[Pi]}},Polygon[{{1,8,9,4},{1,4,3,2},{1,2,5,6},{1,6,7,8},{4,9,12,3},{2,3,11,5},{6,5,13,7},{8,7,14,9},{3,12,10,11},{5,11,10,13},{7,13,10,14},{9,14,10,12}}]]
|>
|>;


(* ::Section:: *)
(*GetLattice*)


(* ::Input::Initialization:: *)
Options[GetLattice]={LatticeConstant->1};
GetLattice::unknownLattice="Unknown lattice identifier `1`. Known lattices are "<>ToString[Keys[data]]<>".";
GetLattice[latticeIdentifier_,opts:OptionsPattern[]]:=Module[{lattice,a,scaled},
If[!KeyExistsQ[data,latticeIdentifier],Message[GetLattice::unknownLattice,latticeIdentifier];Return[]];
lattice=data[latticeIdentifier];
a=OptionValue[LatticeConstant];
scaled=<|
"Basis"->a*lattice["Basis"],
"LatticeVectors"->a*lattice["LatticeVectors"],
"ReciprocalLatticeVectors"->(1/a)*lattice["ReciprocalLatticeVectors"],
"BrillouinZone"->Scale[lattice["BrillouinZone"],1/a]
|>;
If[KeyExistsQ[lattice,"ExtendedBrillouinZone"],scaled["ExtendedBrillouinZone"]=Scale[lattice["ExtendedBrillouinZone"],1/a]];
Return[scaled];
]


(* ::Section:: *)
(*GetBrillouinZone*)


(* ::Input::Initialization:: *)
Options[GetBrillouinZone]=Join[{Type->FirstBZ},Options[GetLattice]];
GetBrillouinZone::noextendedbz="The lattice is a Bravais lattice. The extended Brillouin zone is equivalent with the first Brillouin zone.";
GetBrillouinZone[latticeIdentifier_,opts:OptionsPattern[]]:=Module[{lattice},
lattice=GetLattice[latticeIdentifier,Evaluate[FilterRules[{opts},Options[GetLattice]]]];
Switch[OptionValue[Type],
FirstBZ,Return[lattice["BrillouinZone"]],
ExtendedBZ,If[KeyExistsQ[lattice,"ExtendedBrillouinZone"],Return[lattice["ExtendedBrillouinZone"]],Message[GetBrillouinZone::noextendedbz];Return[lattice["BrillouinZone"]]],
_,Message[GetBrillouinZone::noextendedbz];Return[]
];
]


(* ::Section:: *)
(*GetBrillouinZoneDiscretization*)


(* ::Input::Initialization:: *)
Options[GetBrillouinZoneDiscretization]=Join[{LatticeSize->16,ClippingStyle->Automatic,MaxRecursion->4},Options[GetBrillouinZone]];
GetBrillouinZoneDiscretization::dimerror="The specified lattice is not of dimension 2.";
GetBrillouinZoneDiscretization::invalidclipping="The specified clipping option `1` is invalid. Possible options are {Automatic,None}.";
GetBrillouinZoneDiscretization::noconvergence="Convergence has not been reached, the unit cell may be too anisotropic. Try increasing MaxRecursion.";
GetBrillouinZoneDiscretization[latticeIdentifier_,opts:OptionsPattern[]]:=Module[{q,bz,clip,xmax,ymax,points,maxlvl,newpoints},
q=(1/OptionValue[LatticeSize])*GetLattice[latticeIdentifier,Evaluate[FilterRules[{opts},Options[GetLattice]]]]["ReciprocalLatticeVectors"];
bz=GetBrillouinZone[latticeIdentifier,Evaluate[FilterRules[{opts},Options[GetBrillouinZone]]]];
If[Length[q]=!=2,Message[GetBrillouinZoneDiscretization::dimerror];Return[]];

Switch[OptionValue[ClippingStyle],
Automatic,clip=#\[Element]bz&,
None,(xmax=1.1*Max[#[[1,1]]&/@MeshPrimitives[bz,0]];ymax=1.1*Max[#[[1,2]]&/@MeshPrimitives[bz,0]];clip=#[[1]]>=-xmax&&#[[1]]<=xmax&&#[[2]]>=-ymax&&#[[2]]<=ymax&),
_,Message[GetBrillouinZoneDiscretization::invalidclipping,OptionValue[ClippingStyle]];Return[]
];

points={{0,0}};
maxlvl=OptionValue[MaxRecursion]*OptionValue[LatticeSize];
Do[
newpoints=Join[Table[n1*q[[1]]-lvl*q[[2]],{n1,-lvl,lvl}],Table[n1*q[[1]]+lvl*q[[2]],{n1,-lvl,lvl}],Table[-lvl*q[[1]]+n2*q[[2]],{n2,-lvl+1,lvl-1}],Table[lvl*q[[1]]+n2*q[[2]],{n2,-lvl+1,lvl-1}]];
newpoints=Select[newpoints,clip];
If[Length[newpoints]==0,Break[],points=Join[points,newpoints];If[lvl==maxlvl,Message[GetBrillouinZoneDiscretization::noconvergence]]];
,{lvl,1,maxlvl}];

Return[points];
]


(* ::Section:: *)
(*GetBrillouinZoneDiscretization3D*)


(* ::Input::Initialization:: *)
Options[GetBrillouinZoneDiscretization3D]=Join[{LatticeSize->8,ClippingStyle->Automatic,MaxRecursion->4},Options[GetBrillouinZone]];
GetBrillouinZoneDiscretization3D::dimerror="The specified lattice is not of dimension 3.";
GetBrillouinZoneDiscretization3D::invalidclipping="The specified clipping option `1` is invalid. Possible options are {Automatic,None}.";
GetBrillouinZoneDiscretization3D::noconvergence="Convergence has not been reached, the unit cell may be too anisotropic. Try increasing MaxRecursion.";
GetBrillouinZoneDiscretization3D[latticeIdentifier_,opts:OptionsPattern[]]:=Module[{q,bz,clip,xmax,ymax,zmax,points,maxlvl,newpoints},
q=(1/OptionValue[LatticeSize])*GetLattice[latticeIdentifier,Evaluate[FilterRules[{opts},Options[GetLattice]]]]["ReciprocalLatticeVectors"];
bz=GetBrillouinZone[latticeIdentifier,Evaluate[FilterRules[{opts},Options[GetBrillouinZone]]]];
If[Length[q]=!=3,Message[GetBrillouinZoneDiscretization::dimerror];Return[]];

Switch[OptionValue[ClippingStyle],
Automatic,clip=#\[Element]bz&,
None,(xmax=1.1*Max[#[[1,1]]&/@MeshPrimitives[bz,0]];ymax=1.1*Max[#[[1,2]]&/@MeshPrimitives[bz,0]];zmax=1.1*Max[#[[1,3]]&/@MeshPrimitives[bz,0]];clip=#[[1]]>=-xmax&&#[[1]]<=xmax&&#[[2]]>=-ymax&&#[[2]]<=ymax&&#[[3]]>=-zmax&&#[[3]]<=zmax&),
_,Message[GetBrillouinZoneDiscretization::invalidclipping,OptionValue[ClippingStyle]];Return[]
];

points={{0,0,0}};
maxlvl=OptionValue[MaxRecursion]*OptionValue[LatticeSize];
Do[
newpoints=Flatten[Join[
Table[-lvl*q[[1]]+n2*q[[2]]+n3*q[[3]],{n2,-lvl,lvl},{n3,-lvl,lvl}],
Table[lvl*q[[1]]+n2*q[[2]]+n3*q[[3]],{n2,-lvl,lvl},{n3,-lvl,lvl}],
Table[n1*q[[1]]-lvl*q[[2]]+n3*q[[3]],{n1,-lvl+1,lvl-1},{n3,-lvl,lvl}],
Table[n1*q[[1]]+lvl*q[[2]]+n3*q[[3]],{n1,-lvl+1,lvl-1},{n3,-lvl,lvl}],
Table[n1*q[[1]]+n2*q[[2]]-lvl*q[[3]],{n1,-lvl+1,lvl-1},{n2,-lvl+1,lvl-1}],
Table[n1*q[[1]]+n2*q[[2]]+lvl*q[[3]],{n1,-lvl+1,lvl-1},{n2,-lvl+1,lvl-1}]
],1];
newpoints=Select[newpoints,clip];
If[Length[newpoints]==0,Break[],points=Join[points,newpoints];If[lvl==maxlvl,Message[GetBrillouinZoneDiscretization::noconvergence]]];
,{lvl,1,maxlvl}];

Return[points];
]


(* ::Section:: *)
(*PlotLattice*)


(* ::Input::Initialization:: *)
Options[PlotLattice]=Join[{LatticeRange->4,MaxRecursion->4},Options[GetLattice],Options[Graphics]];
PlotLattice::dimerror="The specified lattice is not of dimension 2.";
PlotLattice::noconvergence="Convergence has not been reached, the unit cell may be too anisotropic. Try increasing MaxRecursion.";
PlotLattice[latticeIdentifier_,opts:OptionsPattern[]]:=Module[{GenerateLattice,lattice,prim,basis,sites,maxlvl,newsites,identifier,plotrule,siteplot,bondplot},
GenerateLattice[id_]:=( 
lattice=GetLattice[id,Evaluate[FilterRules[{opts},Options[GetLattice]]]];
prim=lattice["LatticeVectors"];
basis=lattice["Basis"];
If[Length[basis[[1]]]=!=2,Message[PlotLattice::dimerror];Return[]];

sites=Table[basis[[b]],{b,1,Length[basis]}];
maxlvl=OptionValue[MaxRecursion]*OptionValue[LatticeRange];
Do[
newsites=Flatten[Join[
Table[n1*prim[[1]]-lvl*prim[[2]]+basis[[b]],{n1,-lvl,lvl},{b,1,Length[basis]}],
Table[n1*prim[[1]]+lvl*prim[[2]]+basis[[b]],{n1,-lvl,lvl},{b,1,Length[basis]}],
Table[-lvl*prim[[1]]+n2*prim[[2]]+basis[[b]],{n2,-lvl+1,lvl-1},{b,1,Length[basis]}],
Table[lvl*prim[[1]]+n2*prim[[2]]+basis[[b]],{n2,-lvl+1,lvl-1},{b,1,Length[basis]}]
],1];
newsites=Select[newsites,Norm[#]<=OptionValue[LatticeRange]&];
If[Length[newsites]==0,Break[],sites=Join[sites,newsites];If[lvl==maxlvl,Message[PlotLattice::noconvergence]]]
,{lvl,1,maxlvl}];

siteplot=Point/@sites;
bondplot=Line/@Select[Tuples[sites,2],0.99*OptionValue[LatticeConstant]<= Norm[#[[1]]-#[[2]]]<=1.01OptionValue[LatticeConstant]&];
Return[Join[bondplot,siteplot]];
);

identifier=If[latticeIdentifier[[0]]===String,{latticeIdentifier},latticeIdentifier];
plotrule=If[#[[0]]===String,GenerateLattice[#],#]&/@identifier;
Graphics[plotrule,Evaluate[FilterRules[{opts},Options[Graphics]]],AspectRatio->Automatic]
];


(* ::Section:: *)
(*PlotLattice3D*)


(* ::Input::Initialization:: *)
Options[PlotLattice3D]=Join[{LatticeRange->4,MaxRecursion->4},Options[GetLattice],Options[Graphics3D]];
PlotLattice3D::dimerror="The specified lattice is not of dimension 3.";
PlotLattice3D::noconvergence="Convergence has not been reached, the unit cell may be too anisotropic. Try increasing MaxRecursion.";
PlotLattice3D[latticeIdentifier_,opts:OptionsPattern[]]:=Module[{GenerateLattice,lattice,prim,basis,sites,maxlvl,newsites,identifier,plotrule,siteplot,bondplot},
GenerateLattice[id_]:=( 
lattice=GetLattice[id,Evaluate[FilterRules[{opts},Options[GetLattice]]]];
prim=lattice["LatticeVectors"];
basis=lattice["Basis"];
If[Length[basis[[1]]]=!=3,Message[PlotLattice::dimerror];Return[]];

sites=Table[basis[[b]],{b,1,Length[basis]}];
maxlvl=OptionValue[MaxRecursion]*OptionValue[LatticeRange];
Do[
newsites=Flatten[Join[
Table[-lvl*prim[[1]]+n2*prim[[2]]+n3*prim[[3]]+basis[[b]],{n2,-lvl,lvl},{n3,-lvl,lvl},{b,1,Length[basis]}],
Table[lvl*prim[[1]]+n2*prim[[2]]+n3*prim[[3]]+basis[[b]],{n2,-lvl,lvl},{n3,-lvl,lvl},{b,1,Length[basis]}],
Table[n1*prim[[1]]-lvl*prim[[2]]+n3*prim[[3]]+basis[[b]],{n1,-lvl+1,lvl-1},{n3,-lvl,lvl},{b,1,Length[basis]}],
Table[n1*prim[[1]]+lvl*prim[[2]]+n3*prim[[3]]+basis[[b]],{n1,-lvl+1,lvl-1},{n3,-lvl,lvl},{b,1,Length[basis]}],
Table[n1*prim[[1]]+n2*prim[[2]]-lvl*prim[[3]]+basis[[b]],{n1,-lvl+1,lvl-1},{n2,-lvl+1,lvl-1},{b,1,Length[basis]}],
Table[n1*prim[[1]]+n2*prim[[2]]+lvl*prim[[3]]+basis[[b]],{n1,-lvl+1,lvl-1},{n2,-lvl+1,lvl-1},{b,1,Length[basis]}]
],2];
newsites=Select[newsites,Norm[#]<=OptionValue[LatticeRange]&];
If[Length[newsites]==0,Break[],sites=Join[sites,newsites];If[lvl==maxlvl,Message[PlotLattice::noconvergence]]]
,{lvl,1,maxlvl}];

siteplot=Point/@sites;
bondplot=Line/@Select[Tuples[sites,2],0.99*OptionValue[LatticeConstant]<= Norm[#[[1]]-#[[2]]]<=1.01OptionValue[LatticeConstant]&];
Return[Join[bondplot,siteplot]];
);

identifier=If[latticeIdentifier[[0]]===String,{latticeIdentifier},latticeIdentifier];
plotrule=If[#[[0]]===String,GenerateLattice[#],#]&/@identifier;
Graphics3D[plotrule,Evaluate[FilterRules[{opts},Options[Graphics3D]]],Boxed->False,AspectRatio->Automatic]
];


(* ::Section:: *)
(*PlotBrillouinZone*)


(* ::Input::Initialization:: *)
Options[PlotBrillouinZone]=Join[Options[GetBrillouinZone],Options[Graphics]];
PlotBrillouinZone::dimerror="The specified lattice is not of dimension 2.";
PlotBrillouinZone[latticeIdentifier_,opts:OptionsPattern[]]:=Module[{identifier,plotrule},
identifier=If[latticeIdentifier[[0]]===String,{latticeIdentifier},latticeIdentifier];
plotrule=If[#[[0]]===String,(bz=GetBrillouinZone[#,Evaluate[FilterRules[{opts},Options[GetBrillouinZone]]]];MeshPrimitives[bz,1]),#]&/@identifier;
If[RegionDimension[bz]=!=2,Message[PlotBrillouinZone::dimerror];Return[]];
Graphics[plotrule,Evaluate[FilterRules[{opts},Options[Graphics]]],AspectRatio->Automatic]
];


(* ::Section:: *)
(*PlotBrillouinZone3D*)


(* ::Input::Initialization:: *)
Options[PlotBrillouinZone3D]=Join[Options[GetBrillouinZone],Options[Graphics3D]];
PlotBrillouinZone3D::dimerror="The specified lattice is not of dimension 3.";
PlotBrillouinZone3D[latticeIdentifier_,opts:OptionsPattern[]]:=Module[{identifier,plotrule},
identifier=If[latticeIdentifier[[0]]===String,{latticeIdentifier},latticeIdentifier];
plotrule=If[#[[0]]===String,(bz=GetBrillouinZone[#,Evaluate[FilterRules[{opts},Options[GetBrillouinZone]]]];MeshPrimitives[bz,1]),#]&/@identifier;
If[RegionDimension[bz]=!=3,Message[PlotBrillouinZone3D::dimerror];Return[]];
Graphics3D[plotrule,Evaluate[FilterRules[{opts},Options[Graphics3D]]],Boxed->False,AspectRatio->Automatic]
];


(* ::Chapter:: *)
(*End implementation*)


End[];
EndPackage[];
