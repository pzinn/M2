-----------------------
-------MAIN------------
-----------------------

doc ///
  Key
    Bertini
  Headline
    software for numerical algebraic geometry
  Description
    Text      
      Interfaces the functionality of the software {\tt Bertini}
      to solve polynomial systems and perform calculations in
      {\em numerical algebraic geometry}. The software is available at
      @HREF"http://www.nd.edu/~sommese/bertini/"@.

      The user may place the executable program {\tt bertini} in the executation path. 
      Alternatively, the path to the executable needs to be specified, for instance,
    Example
      needsPackage("Bertini", Configuration=>{"BERTINIexecutable"=>"/folder/subfolder/bertini"})  
    Text
      Below is a simple example using the most popular function,
      a basic zero-dimensional solve with no special options.
    Example
      R = CC[x,y]
      F = {x^2-1,y^2-1}
      solns = bertiniZeroDimSolve(F)
///;

---------------------------
------FUNCTIONS------------
---------------------------

doc ///
 Key
   makeB'InputFile
   (makeB'InputFile,String)
 Headline
   write a Bertini input file in a directory
 Usage
   makeB'InputFile(s) 
 Inputs
   s:String
     a directory where the input file will be written
 Description
   Text
     This function writes a Bertini input file. 
     The user can specify CONFIGS for the file using the B'Configs option.
     The user should specify variable groups with the AVG (affine variable group) option or HVG (homogenous variable group) option. 
     The user should specify the polynomial system they want to solve with the  B'Polynomials option or B'Functions option.
     If B'Polynomials is not used then the user should use the  NamePolynomials option. 
   Example
     R=QQ[x1,x2,y]
     theDir = temporaryFileName()
     makeDirectory theDir
     makeB'InputFile(theDir,
	 B'Configs=>{{"MPTYPE",2}},
     	 AVG=>{{x1,x2},{y}},
	 B'Polynomials=>{y*(x1+x2+1)^2+1,x1-x2+1,y-2})
   Example
     R=QQ[x1,x2,y,X]
     makeB'InputFile(theDir,
	 B'Configs=>{{"MPTYPE",2}},
     	 AVG=>{{x1,x2},{y}},
	 NamePolynomials=>{f1,f2,f3},
	 B'Functions=>{
	     {X,x1+x2+1},
	     {f1,y*X^2+1},
	     {f2,x1-x2+1},
	     {f3,y-2}})     
   Example
     R=QQ[x1,x2,y,X]
     makeB'InputFile(theDir,
	 B'Configs=>{{"MPTYPE",2}},
     	 AVG=>{{x1,x2},{y}},
	 B'Polynomials=>{y*X^2+1,x1-x2+1,y-2},
	 B'Functions=>{
	     {X,x1+x2+1}})          
 Caveat
   Variables must begin with a letter (lowercase or capital) and
   can only contain letters, numbers, underscores, and square brackets.
   "jade" should not be used in any expression. 
   "I" can only be used to represent the complex number sqrt(-1).
      
///;


doc ///
 Key
   b'PHSequence
   (b'PHSequence,String,List)
 Headline
   b'PHSequence performs a sequence of parameter homotopies.
 Usage
   b'PHSequence(s,l) 
 Inputs
   s:String
     The directory where the files are stored.
   l:List
     A list of list of parameter values.
 Description
   Text
     The string s is a directory where the Bertini files are stored. 
     A Bertini input file should be stored in s, or the InputFileDirectory should be set to a string specifiying where an input file can be found. 
     The Bertini input file should be named "input", or the NameB'InputFile should be set to a string giving the name of the input file. 
     A Bertini start (solutions) file should be stored in s, or the StartFileDirectory should be set to a string specifiying where a start file can be found. 
     The Bertini start (solutions) file should be named "start", or the NameStartFile should be set to a string giving the name of the start file. 
     A Bertini start parameter file should be stored in s, or the StartParameterFileDirectory should be set to a string specifiying where a start parameter file can be found. 
     The Bertini start parameter file should be named "start_parameters", or the NameParameterFile should be set to a string giving the name of the start parameter file. 
     b'PHSequence loops through the elements of l doing a sequence of parameter homotopies.
     Each element of l gives the parameter values  for a parameter homotopy.
     These parameter values are written to a "final_parameters" file in s. 
     Bertini is called (the option B'Exe allows one to change the way in which Bertini is called).
     The resulting nonsingular_solutions and final parameters become start solutions and start parameters for the next parameter homotopy in the sequence.  
     
   Example
     R=QQ[x,y,t1,t2]
     makeB'InputFile(storeBM2Files, PG=>{t1,t2},AVG=>{{x,y}},	 
	 B'Configs=>{{"PARAMETERHOMOTOPY",1}},
	 B'Polynomials=>{x^2-t1,y-t2})
     runBertini(storeBM2Files)
     moveFile(storeBM2Files|"/finite_solutions",storeBM2Files|"/start")
     makeB'InputFile(storeBM2Files, PG=>{t1,t2},AVG=>{{x,y}},	 
	 B'Configs=>{{"PARAMETERHOMOTOPY",2}},
	 B'Polynomials=>{x^2-t1,y-t2})
     b'PHSequence(storeBM2Files,{{1,1},{1,2}},SaveData=>true)
      
///;


doc ///
 Key
   importSolutionsFile
   (importSolutionsFile,String)
 Headline
   Imports coordinates from a Bertini solution file.
 Usage
   importSolutionsFile(s) 
 Inputs
   s:String
     The directory where the file is stored.
 Description
   Text
     After Bertini does a run many files are created. 
     This function imports the coordinates of solutions from the simple "raw_solutions" file. 
     By using the option NameSolutionsFile=>"real_finite_solutions" we would import solutions from real finite solutions. 
     Other common file names are "nonsingular_solutions", "finite_solutions", "infinite_solutions", and "singular_solutions".     
   Text
     If the NameSolutionsFile option 
     is set to 0 then "nonsingular_solutions" is imported,
     is set to 1 then "real_finite_solutions" is imported,
     is set to 2 then "infinite_solutions" is imported,
     is set to 3 then "finite_solutions" is imported,
     is set to 4 then "start" is imported,
     is set to 5 then "raw_solutions" is imported.
   Example
     R=QQ[x,y]
     makeB'InputFile(storeBM2Files,
     	 AVG=>{{x,y}},
	 B'Polynomials=>{x^2-1,y^3-1})
     runBertini(storeBM2Files)
     importSolutionsFile(storeBM2Files)
     importSolutionsFile(storeBM2Files,NameSolutionsFile=>"real_finite_solutions")
     importSolutionsFile(storeBM2Files,NameSolutionsFile=>0)     
      
///;


doc ///
 Key
   importIncidenceMatrix
   (importIncidenceMatrix,String)
 Headline
   Imports an incidence matrix file after running makeMembershipFile.
 Usage
   importIncidenceMatrix(s) 
 Inputs
   s:String
     The directory where the file is stored.
 Description
   Text
     After running makeMembershipFile Bertini produces an incidence_matrix file. 
     The incidence_matrix says which points belong to which components.
   Text
     If the NameIncidenceMatrixFile option is set when we want to import files with a different name.
   Example
    makeB'InputFile(storeBM2Files,
    	B'Configs=>{{TrackType,1}},    AVG=>{x,y,z},    B'Polynomials=>{"z*((x+y+z)^3-1)","z*(y^2-3+z)"}    )
    runBertini(storeBM2Files)
    makeSamplePointFile(storeBM2Files,1,0,2)
    makeMembershipFile(storeBM2Files,NameWitnessPointFile=>"sample_point_file")
    importIncidenceMatrix(storeBM2Files)
      
///;


doc ///
 Key
   makeMembershipFile
   (makeMembershipFile,String)
 Headline
   Creats a Bertini incidence_matrix file using Tracktype 3..
 Usage
   makeMembershipFile(s) 
 Inputs
   s:String
     The directory where the input file, witness_data, and member_points is stored. 
 Description
   Text
     After doing a positive dimensional solve with Bertini a witness data file is produced.
     This function requires an input file, member_points file and witness_data file. 
     It appends "TrackType: 3" to the configurations in the input file and calls Bertini to produce and incidence_matrix file with respect to the member_points file.
   Text
     The option NameWitnessPointFile has "member_points" set as its default. 
   Text
     The option TestSolutions can be set to a list of coordinates of points which will be written to a file. 
   Example
    makeB'InputFile(storeBM2Files,
    	B'Configs=>{{TrackType,1}},    AVG=>{x,y,z},    B'Polynomials=>{"z*((x+y+z)^3-1)","z*(y^2-3+z)"}    )
    runBertini(storeBM2Files)
    makeSamplePointFile(storeBM2Files,1,0,2)
    makeMembershipFile(storeBM2Files,NameWitnessPointFile=>"sample_point_file")
    makeMembershipFile(storeBM2Files,TestSolutions=>{{1,2,0},{3,2,1}})
    importIncidenceMatrix(storeBM2Files)
      
///;






doc ///
 Key
   runBertini
   (runBertini,String)
 Headline
   Calls Bertini.
 Usage
   runBertini(s) 
 Inputs
   s:String
     The directory where Bertini will store files.
 Description
   Text
     To run bertini we need to say where we want to output the files, which is given by s. 
     Additional options include speciifying the location of the input file (the default is that the input file is located where we output the files)
     B'Exe is how we call Bertini. The default option is how Bertini is usually called in M2 in the init file. 
     InputFileName has its default as "input". 
   Example
     R=QQ[x,y]
     makeB'InputFile(storeBM2Files,
     	 AVG=>{{x,y}},
	 B'Polynomials=>{x^2-1,y^2-4})
     runBertini(storeBM2Files)
   Example
     R=QQ[x,y]
     theDir1 = temporaryFileName()
     makeDirectory theDir1
     theDir2 = temporaryFileName()
     makeDirectory theDir2
     makeB'InputFile(theDir1,
     	 AVG=>{{x,y}},
	 B'Polynomials=>{x^2-1,y^2-4})
     runBertini(theDir2,InputFileDirectory=>theDir1)
      
///;


doc ///
 Key
   makeWitnessSetFile
   (makeWitnessSetFile,String,Number)
 Headline
   This function creates a witness point file and a slice file. 
 Usage
   makeWitnessSetFile(theDir,d) 
 Inputs
   theDir:String
     The directory where Bertini will store files and the witness_data file and input file are located.
   d:Number 
     The dimension of the variety that we intersect with the slice defined by the linear system of equations.
 Description
   Text
     This function does a track type 4 Bertini run creating a linear system file. The slice information for a positive dimensional run can be recovered from such a file.     
   Example
     makeB'InputFile(storeBM2Files,
       AVG=>{x,y,z},
       B'Configs=>{{TrackType,1}},
       B'Polynomials=>{"(x^2+y^2+z^2-1)*y"})
     runBertini(storeBM2Files)
     makeWitnessSetFile(storeBM2Files,2)--creats a witness point file for all dimension 2 components and a linear slice file for dimension 2 components. 
     L=importSliceFile(storeBM2Files) 
--
     makeWitnessSetFile(storeBM2Files,2,
       NameWitnessPointFile=>"custom_name_witness_points",--creates a witness point file with a custom name. 
       SpecifyComponent=>0)  --Component indexing begins at 0. The function creates a witness point file for only a particular component. 
     L1=importSliceFile(storeBM2Files) 
      
///;

doc ///
 Key
   makeSamplePointFile
   (makeSamplePointFile,String,Number,Number,Number)
 Headline
   This function samples points from a component by performing a Bertini TrackType 2.
 Usage
   makeSamplePointFile(theDir,d,c,s) 
 Inputs
   theDir:String
     The directory where Bertini will store files and the witness_data file and input file are located.
   d:Number 
     The dimension of the component that we want to sample from.
   c:Number
     The component number that we will sample from (indexing starts at 0).
 Description
   Text
     This function does a track type 2 Bertini run creating "sample_point_file" that contains a list of witness points in the standard Bertini format.     
   Example
     makeB'InputFile(storeBM2Files,
       AVG=>{x,y,z},
       B'Configs=>{{TrackType,1}},
       B'Polynomials=>{"(x^2+y^2+z^2-1)*y"})
     runBertini(storeBM2Files)
     makeSamplePointFile(storeBM2Files,2,0,100)--creates a witness point file with 100 sample points for the 0th component in dimension 2. 
     theSols=importSolutionsFile(storeBM2Files,NameSolutionsFile=>"sample_point_file") 
      
///;

doc ///
 Key
   importMainDataFile
   (importMainDataFile,String)
 Headline
   This function imports points from the main data file form a Bertini run.
 Usage
   importMainDataFile(theDir) 
 Inputs
   theDir:String
     The directory where the main_data file is located.
 Description
   Text
     This function does not import a list of coordinates. Instead it imports points from a main_data file. These points contain coordinates, condition numbers, and etc.      
     The information the points contain depend on if regeneration was used and if a TrackType 0 or 1 was used.    
   Example
     makeB'InputFile(storeBM2Files,
       AVG=>{x,y,z},
       B'Configs=>{{TrackType,1}},
       B'Polynomials=>{"(x^2+y^2+z^2-1)*y"})
     runBertini(storeBM2Files)
     thePoints=importMainDataFile(storeBM2Files)
     witnessPointsDim1= importMainDataFile(storeBM2Files,SpecifyDim=>1)--We can choose which dimension we import points from. There are no witness points in dimension 1.
      
///;


doc ///
 Key
   writeParameterFile
   (writeParameterFile,String,List)
 Headline
   Writes the list of parameter values to a file that Bertini can read. 
 Usage
   writeParameterFile(s,v) 
 Inputs
   s:String
     The directory where the Bertini file will be written.
   v:List
     A list of numbers that will be written to the file.   
 Description
   Text
     To do a paremeter homotopy one must have a start_parameters file and a final_parameters file. 
   Example
     R=QQ[x,y,t]
     makeB'InputFile(storeBM2Files,
     	 B'Configs=>{{"PARAMETERHOMOTOPY",1}},
	 PG=>{t},    AVG=>{{x,y}},
	 B'Polynomials=>{x^2-1,y^2-t})
     runBertini(storeBM2Files)
     copyFile(storeBM2Files|"/nonsingular_solutions",storeBM2Files|"/start")
     makeB'InputFile(storeBM2Files,
     	 B'Configs=>{{"PARAMETERHOMOTOPY",2}},
	 PG=>{t},    AVG=>{{x,y}},
	 B'Polynomials=>{x^2-1,y^2-t})
     writeParameterFile(storeBM2Files,{1})
     runBertini(storeBM2Files)

///;


doc ///
 Key
   writeStartFile
   (writeStartFile,String,List)
 Headline
   Writes the list of list of coordinates to a file that Bertini can read. 
 Usage
   writeStartFile(s,v) 
 Inputs
   s:String
     The directory where the Bertini file will be written.
   v:List
     A list of list numbers that will be written to the file.   
 Description
   Text
     This function can be used to write "start" files and any other solution file using the option NameStartFile=>"AnyNameYouWant". 
   Example
     coordinatesOfTwoPnts={{1,0},{3,4}}
     writeStartFile(storeBM2Files,coordinatesOfTwoPnts)

///;

doc ///
 Key
   NumberToB'String
   (NumberToB'String,Thing)
 Headline
   Translates a number to a string that Bertini can read. 
 Usage
   NumberToB'String(n) 
 Inputs
   n:Thing
     n is a number.
 Description
   Text
     This function takes a number as an input then outputs a string to represent this number to Bertini.
     The numbers are converted to floating point to precision determined by the option UsePrecision.       
   Example
     NumberToB'String(2+5*ii)
     NumberToB'String(1/3,UsePrecision=>16)
     NumberToB'String(1/3,UsePrecision=>128)

///;


doc ///
 Key
   bertiniZeroDimSolve
   (bertiniZeroDimSolve,List)
 Headline
   solve zero-dimensional system of equations
 Usage
   S = bertiniZeroDimSolve F
 Inputs
   F:List
     whose entries are polynomials (system need not be square)
 Outputs
   S:List
     of solutions of type Point    
 Description
   Text
     Finds solutions to the zero-dimensional system F via numerical polynomial homotopy continuation.
     This function builds a Bertini input file from the system F and calls Bertini on
     this input file. Solutions are pulled from machine readable file {\tt finitesolutions}
     and returned as a list.
   Example
     R = CC[x,y];
     F = {x^2-1,y^2-1};
     S = bertiniZeroDimSolve F
   Text
     Each solution is of type @TO Point@.  Additional information about the solution can be accessed by using @TO peek@. 
   Example
     peek S_0 
 Caveat
   Variables must begin with a letter (lowercase or capital) and
   can only contain letters, numbers, underscores, and square brackets.
      
///;



doc ///
 Key
   bertiniPosDimSolve
   (bertiniPosDimSolve,List)
 Headline
   solve positive-dimensional system of equations
 Usage
   V = bertiniPosDimSolve F
 Inputs
   F:List
     whose entries are polynomials 
 Outputs
   V:NumericalVariety 
     a numerical description of irreducible components of the variety defined by F
 Consequences
   Item
     Writes the system to temporary files
   Item
     Invokes {\tt Bertini}'s solver with {\tt TRACKTYPE: 1}. Bertini uses a
      cascade homotopy to find witness supersets in each dimension, removes extra
      points using a membership test or local dimension test, deflates singular
      witness points, then factors using a combination of monodromy and a linear trace
      test 
   Item
     Stores output of {\tt Bertini} in temporary file
   Item
     Parses and outputs the solutions       
 Description
   Text
     The method {\tt bertiniPosDimSolve} calls  {\tt Bertini} to find
     a numerical irreducible decomposition of the zero-set of F.  The decomposition is
     returned as the @TO NumericalVariety@ NV.  Witness sets of NV contain approximations
     to solutions of the system F=0. 
   Example
     R = QQ[x,y,z]
     F = {(y^2+x^2+z^2-1)*x,(y^2+x^2+z^2-1)*y}
     S = bertiniPosDimSolve F
     S#1_0#Points -- 1_0 chooses the first witness set in dimension 1
   Text
     Each @TO WitnessSet@ is accessed by dimension and then list position.
   Example
     S#1 --first specify dimension
     peek oo_0 --then list position   
   Text
     In the example, we find two components, one component has dimension 1 and degree 1 and the other has
     dimension 2 and degree 2.  We get the same results using symbolic methods.
   Example
     PD=primaryDecomposition( ideal F)
     dim PD_0
     degree PD_0
     dim PD_1
     degree PD_1
 Caveat
   Variables must begin with a letter (lowercase or capital) and
   can only contain letters, numbers, underscores, and square brackets.            
///;



doc ///
 Key
   bertiniSample
   (bertiniSample, ZZ, WitnessSet)
 Headline
   sample points from an irreducible component of a variety
 Usage
   V = bertiniSample (n, W)
 Inputs
   n:ZZ
     number of desired sample points
   W:WitnessSet
     irreducible
 Outputs
   L:List
     sample points
 Consequences
  Item
    Writes the witness set to a temporary file
  Item
    Invokes {\tt Bertini}'s solver with option {\tt TRACKTYPE: 2}. 
    To sample, {\tt Bertini} moves the hyperplannes defined in the @TO WitnessSet@ W within
    the space until the desired points are sampled.
  Item
    Stores output of {\tt Bertini} in temporary file
  Item
    Parses and outputs the solutions    
 Description
   Text
     Samples points from an irreducible component of a variety using Bertini.  The irreducible
     component needs to be in its numerical form as a @TO WitnessSet@.  The method
     @TO bertiniPosDimSolve@ can be used to generate a witness set for the component.
   Example
     R = CC[x,y,z]     
     F = { (y^2+x^2+z^2-1)*x, (y^2+x^2+z^2-1)*y }
     NV = bertiniPosDimSolve(F)
     W = NV#1_0 --z-axis
     bertiniSample(4, W)
///;

doc ///
 Key
   bertiniTrackHomotopy
   (bertiniTrackHomotopy, RingElement, List, List)
 Headline
   track a user-defined homotopy
 Usage
   S0=bertiniTrackHomotopy(t, H, S1)
 Inputs
   t:RingElement
     path variable
   H:List
     of polynomials that define the homotopy
   S1:List
     of solutions to the start system  
 Outputs
   S0:List
     of solutions to the target system
 Consequences
  Item
    Writes the homotopy and start solutions to temporary files
  Item
    Invokes {\tt Bertini}'s solver with configuration keyword {\tt USERHOMOTOPY}. 
  Item
    Stores output of {\tt Bertini} in temporary file
  Item
    Parses and outputs the solutions       
 Description
   Text
     This method calls {\tt Bertini} to track a user-defined homotopy.  The
     user needs to specify the homotopy H, the path variable t, and a list
     of start solutions S1. In the following example, we solve $x^2-2$ by moving
     from $x^2-1$ with a linear homotopy. {\tt Bertini} tracks homotopies starting at
     $t=1$ and ending at $t=0$. Final solutions are of type Point.
   Example
     R = CC[x,t]; -- include the path variable in the ring     
     H = { (x^2-1)*t + (x^2-2)*(1-t)};
     sol1 = point {{1}};
     sol2 = point {{-1}};
     S1= { sol1, sol2  };--solutions to H when t=1	  
     S0 = bertiniTrackHomotopy (t, H, S1) --solutions to H when t=0
     peek S0_0
   Example     
     R=CC[x,y,t]; -- include the path variable in the ring     
     f1=(x^2-y^2);
     f2=(2*x^2-3*x*y+5*y^2);
     H = { f1*t + f2*(1-t)}; --H is a list of polynomials in x,y,t
     sol1=    point{{1,1}}--{{x,y}} coordinates
     sol2=    point{{ -1,1}}
     S1={sol1,sol2}--solutions to H when t=1
     S0=bertiniTrackHomotopy(t, H, S1, ISPROJECTIVE=>1) --solutions to H when t=0 
 Caveat
   Variables must begin with a letter (lowercase or capital) and
   can only contain letters, numbers, underscores, and square brackets.           
///;

doc ///
 Key
   bertiniComponentMemberTest
   (bertiniComponentMemberTest, List, NumericalVariety)
 Headline
   test whether points lie on a given variety
 Usage
   L = bertiniComponentMemberTest (pts, NV)
 Inputs
   pts:List
     points to test
   NV:NumericalVariety
 Outputs
   L:List
     entries are lists of witness sets containing the test point
 Consequences
  Item
    Writes the witness set information of NV and the test points to temporary files
  Item
    Invokes {\tt Bertini}'s solver with option {\tt TRACKTYPE: 3} 
  Item
    Stores output of {\tt Bertini} in temporary file
  Item
    Parses and outputs the solutions    
 Description
   Text
     This method checks whether the test points pts lie on NV using {\tt Bertini}.  
   Example
     R = CC[x,y,z];
     F = {(y^2+x^2+z^2-1)*x,(y^2+x^2+z^2-1)*y};
     NV = bertiniPosDimSolve(F)
     pts = {{0,0,0}} --z-axis
     bertiniComponentMemberTest(pts, NV)
 Caveat
   In the current implementation, at most one witness set is listed for each test point although the point may lie on more than one component.
///;

doc ///
 Key
   bertiniRefineSols
   (bertiniRefineSols, ZZ, List, List)
 Headline
   sharpen solutions to a prescribed number of digits
 Usage
   S = bertiniRefineSols(d, F, l)
 Inputs
   d:ZZ
     number of digits
   F:List
     whose entries are polynomials (system need not be square)
   l:List
     whose entries are points to be sharpened   
 Outputs
   S:List
     of solutions of type Point
 Description
   Text
     This method takes the list l of solutions of F and sharpens them to d digits using the sharpening module of {\tt Bertini}.
   Example
     R = CC[x,y];
     F = {x^2-2,y^2-2};
     sols = bertiniZeroDimSolve (F)
     S = bertiniRefineSols (100, F, sols)
     coords = coordinates S_0
     coords_0
 Caveat
   @TO bertiniRefineSols@ will only refine non-singular solutions and does not
   currently work for homogeneous systems.  
///;

doc ///
 Key
   bertiniParameterHomotopy
   (bertiniParameterHomotopy,List,List,List)
 Headline
   run parameter homotopy in Bertini
 Usage
   S = bertiniParameterHomotopy(F,P,T)
 Inputs
   F:List
     whose entries are polynomials (system need not be square)
   P:List
     parameter names
   T:List
     whose entries are lists of target parameter values
   
 Outputs
   S:List
     whose entries are lists of solutions for each target system
 Consequences
  Item
    Writes systems to temporary files
  Item
    Invokes {\tt Bertini}'s solver with configuration keyword {\tt PARAMETERHOMOTOPY}.
    First with {\tt PARAMETERHOMOTOPY} set to 1, then with {\tt PARAMETERHOMOTOPY} set to
    2 for each set of parameter values.
  Item
    Stores output of {\tt Bertini} in temporary files
  Item
    Parses and outputs the solutions    
 Description
   Text
     This method numerically solves several polynomial systems from
     a parameterized family at once.  The list F is a system of polynomials
     in ring variables and the parameters listed in P.  The list T is the
     set of parameter values for which solutions to F are desired.  Both stages of
     {\tt Bertini}'s parameter homotopy method are called with {\tt bertiniParameterHomotopy}. 
     First, {\tt Bertini} assigns a random complex number to each parameter
     and solves the resulting system, then, after this initial phase, {\tt Bertini} computes solutions
     for every given choice of parameters using a number of paths equal to the exact root count in the
     first stage.
   Example
     R=CC[u1,u2,u3,x,y];
     f1=u1*(y-1)+u2*(y-2)+u3*(y-3); --parameters are u1, u2, and u3
     f2=(x-11)*(x-12)*(x-13)-u1;
     paramValues0={{1,0,0}};
     paramValues1={{0,1+2*ii,0}};
     bPH=bertiniParameterHomotopy( {f1,f2}, {u1,u2,u3},{paramValues0 ,paramValues1 })
     bPH_0--the solutions to the system with parameters set equal to paramValues0
   Example
     R=CC[x,y,z,u1,u2]
     f1=x^2+y^2-z^2
     f2=u1*x+u2*y
     finalParameters0={{0,1}}
     finalParameters1={{1,0}}
     bPH=bertiniParameterHomotopy( {f1,f2}, {u1,u2},{finalParameters0 ,finalParameters1 },ISPROJECTIVE=>1)            
     bPH_0--The two solutions for finalParameters0
 Caveat
   Variables must begin with a letter (lowercase or capital) and
   can only contain letters, numbers, underscores, and square brackets.     
///;

-------------------
-----OPTIONS-------
-------------------
doc ///
  Key
   [bertiniTrackHomotopy, Verbose]
   [bertiniParameterHomotopy, Verbose]
   [bertiniComponentMemberTest, Verbose]
   [bertiniPosDimSolve, Verbose]
   [bertiniRefineSols, Verbose]
   [bertiniSample, Verbose]
   [bertiniZeroDimSolve, Verbose]
  Headline
    Option to silence additional output 
  Usage
    bertiniTrackHomotopyVerbose(...,Verbose=>Boolean)
    bertiniParameterHomotopy(...,Verbose=>Boolean)
    bertiniPosDimSolve(...,Verbose=>Boolean)
    bertiniRefineSols(...,Verbose=>Boolean)
    bertiniSample(...,Verbose=>Boolean)
    bertiniZeroDimSolve(...,Verbose=>Boolean)
  Description
    Text
       Use {\tt Verbose=>false} to silence additional output.
///;

doc ///
 Key
   ISPROJECTIVE
   [bertiniTrackHomotopy, ISPROJECTIVE]
   [bertiniParameterHomotopy, ISPROJECTIVE]
   [bertiniComponentMemberTest, ISPROJECTIVE]
   [bertiniPosDimSolve, ISPROJECTIVE]
   [bertiniRefineSols, ISPROJECTIVE]
   [bertiniSample, ISPROJECTIVE]
   [bertiniZeroDimSolve, ISPROJECTIVE]
 Headline
   optional argument to specify whether to use homogeneous coordinates
 Description
   Text
     When set to 1, this option indicates that the input system is homogenized and
     the output should be given in projective space.
   Example
     R = CC[x,y,z];
     f = {(x^2+y^2-z^2)*(z-x),(x^2+y^2-z^2)*(z+y)};
     bertiniPosDimSolve(f,ISPROJECTIVE=>1);
     
   Example 
     R=CC[x,y,z,u1,u2];--u1,u2 are parameters
     f1=x^2+y^2-z^2;
     f2=u1*x+u2*y;
     finalParameters={{0,1}};
     bPH=bertiniParameterHomotopy( {f1,f2}, {u1,u2},{finalParameters },ISPROJECTIVE=>1)            
///;


doc///
 Key
   MultiplicityTol
   [bertiniZeroDimSolve, MultiplicityTol]
 Headline
   numerical tolerance for grouping solutions   
 Description
   Text
     Solutions are grouped using
     @TO solutionsWithMultiplicity@ from the package @TO NAGtypes@; the option @TO MultiplicityTol@
     is passed to @TO solutionsWithMultiplicity@ as @TO Tolerance@.  The default value of @TO MultiplicityTol@
     is 1e-6. 
///;

doc///
 Key
   AVG
   [makeB'InputFile, AVG]
 Headline
   An option which designates the Affine Variable Groups.    
 Description
   Text
     We can group variables together when using zero-dimensional runs in Bertini. 
   Example
     R=QQ[x1,x2,y]
     makeB'InputFile(storeBM2Files,
	 B'Configs=>{{"MPTYPE",2}},
     	 AVG=>{{x1,x2},{y}},
	 B'Polynomials=>{y*(x1+x2+1)^2+1,x1-x2+1,y-2})
     
///;

doc///
 Key
   HVG
   [makeB'InputFile, HVG]
 Headline
   An option which designates the Homogeneous Variable Groups.    
 Description
   Text
     We can group variables together when using zero-dimensional runs in Bertini. 
   Example
     R=QQ[x0,x1,y0,y1,z]
     makeB'InputFile(storeBM2Files,
	 B'Configs=>{{"MPTYPE",2}},
     	 HVG=>{{x0,x1},{y0,y1}},
	 AVG=>{{z}},
	 B'Polynomials=>{z*x1^2+x0^2,y0*z+y1,y0-2*z^2*y1})
     
///;

doc///
 Key
   PG
   [makeB'InputFile, PG]
 Headline
   An option which designates the parameters for a Parameter Homotopy.    
 Description
   Text
     This option should be set to a list of parameters. 
   Example
     R=QQ[x,y,u]
     makeB'InputFile(storeBM2Files,
	 B'Configs=>{{"PARAMETERHOMOTOPY",1}},
	 AVG=>{{x,y}},
	 PG=>{u},
	 B'Polynomials=>{y-(x^2-1),y-u})
     
///;

doc///
 Key
   B'Configs
   [makeB'InputFile, B'Configs]
 Headline
   An option to designate the CONFIG part of a Bertini Input file.
 Description
   Text
     This option should be set to a list of lists of 2 elements. The first element is the name of the Bertini option, e.g. "MPTYPE" and and the second element is what the Bertini option will be set to e.g. "2".
   Example
     R=QQ[x0,x1,y0,y1,z]
     makeB'InputFile(storeBM2Files,
	 B'Configs=>{{"MPTYPE",2}},
     	 HVG=>{{x0,x1},{y0,y1}},
	 AVG=>{{z}},
	 B'Polynomials=>{z*x1^2+x0^2,y0*z+y1,y0-2*z^2*y1})
     
///;


doc///
 Key
   B'Constants
   [makeB'InputFile, B'Constants]
 Headline
   An option to designate the constants for a Bertini Input file.
 Description
   Text
     This option should be set to a list of lists of 2 elements. 
     The first element is the name of the Constant.
     The second element is the value that the consant will be set.
   Example
     R=QQ[z,a,b,c]
     makeB'InputFile(storeBM2Files,
	 B'Configs=>{{"MPTYPE",2}},
	 AVG=>{{z}},
	 B'Constants=>{{a,2},{b,3+2*ii},{c,3/2}},
	 B'Polynomials=>{a*z^2+b*z+c})
     
///;


doc///
 Key
   RandomComplex
   [makeB'InputFile, RandomComplex]
 Headline
   An option which designates symbols/strings/variables that will be set to be random complex numbers.
 Description
   Text
     This option should be set to a list of symbols, strings, or variables. 
     Elemenets of this list will be set to random complex numbers when Bertini is called.
   Example
     R=QQ[x,y,c1,c2]
     makeB'InputFile(storeBM2Files,
	 AVG=>{{x,y}},
	 RandomComplex=>{c1,c2},--c1=.1212+ii*.1344, c2=.4132-ii*.2144 are written to the input file.
	 B'Polynomials=>{x-c1,y-c2})
   Text
     AFTER Bertini is run, the random values are stored in a file named "random_values".
     
///;

doc///
 Key
   RandomReal
   [makeB'InputFile, RandomReal]
 Headline
   An option which designates symbols/strings/variables that will be set to be a random real number.
 Description
   Text
     This option should be set to a list of symbols, strings, or variables. 
     Elemenets of this list will be set to random real numbers when Bertini is called.
   Example
     R=QQ[x,y,c1,c2]
     makeB'InputFile(storeBM2Files,
	 AVG=>{{x,y}},
	 RandomReal=>{c1,c2},--c1=.1212, c2=.4132 may be written to the input file.
	 B'Polynomials=>{x-c1,y-c2})
   Text
     AFTER Bertini is run, the random values are stored in a file named "random_values".
     
///;


doc///
 Key
   B'Polynomials
   [makeB'InputFile, B'Polynomials]
 Headline
   An option which designates the polynomials that we want to solve.    
 Description
   Text
     The user should specify the polynomial system they want to solve with the  B'Polynomials option or B'Functions option.
     If B'Polynomials is not used then the user should use the  NamePolynomials option. 
   Example
     R=QQ[x,y]
     makeB'InputFile(storeBM2Files,
	 B'Configs=>{{"MPTYPE",2}},
	 AVG=>{{x,y}},
	 B'Polynomials=>{x+y-1,x^2-2})
   Text
     B'Polynomials can be in combination with B'Functions. B'Functions allows the user to define subfunctions.  
   Example
     R=QQ[x,y,A]
     makeB'InputFile(storeBM2Files,
	 AVG=>{{x,y}},
	 B'Functions=>{{A,x^2-1}},
	 B'Polynomials=>{A+y,x+y-2})
     
///;


doc///
 Key
   B'Functions
   [makeB'InputFile, B'Functions]
 Headline
   An option which designates sub-functions or a polynomial system as a straight line program.  
 Description
   Text
     The user should specify the polynomial system they want to solve with the  B'Polynomials option or the B'Functions option.
     The user should use the  NamePolynomials option in conjunction with B'Functions whenever B'Polynomials is not used. 
   Example
     R=QQ[x,y]
     makeB'InputFile(storeBM2Files,
	 AVG=>{{x,y}},
	 NamePolynomials=>{f1,f2},
	 B'Functions=>{{f1,x+y-1},{f2,x^2-2}})--f1=x+y+1,f2=x^2-2 is written to the input file
   Text
     B'Polynomials can be in combination with B'Functions. B'Functions allows the user to define subfunctions.  
   Example
     R=QQ[x,y,A]
     makeB'InputFile(storeBM2Files,
	 AVG=>{{x,y}},
	 B'Functions=>{{A,x^2-1}},--A=x^2-1 is written to the input file
	 B'Polynomials=>{A+y,x+y-2})
     
///;


doc///
 Key
   NamePolynomials
   [makeB'InputFile, NamePolynomials]
 Headline
   An option which designates the names of the polynomials we want to solve.  
 Description
   Text
     The user should specify the polynomial system they want to solve with the  B'Polynomials option or the B'Functions option.
     The user should use the  NamePolynomials option in conjunction with B'Functions whenever B'Polynomials is not used. 
   Example
     R=QQ[x,y]
     makeB'InputFile(storeBM2Files,
	 AVG=>{{x,y}},
	 NamePolynomials=>{f1,f2},
	 B'Functions=>{{f1,x+y-1},{f2,x^2-2}})--f1=x+y+1,f2=x^2-2 is written to the input file
     
///;


doc///
 Key
   NameB'InputFile
   [makeB'InputFile, NameB'InputFile]
 Headline
   This option names the input file. 
 Description
   Text
     Set this option to a string to name the input file. 
   Example
     R=QQ[x,y]
     makeB'InputFile(storeBM2Files,
	 AVG=>{{x,y}},
	 B'Polynomials=>{x+y-1,x^2-2},
	 NameB'InputFile=>"testInput")--the input file will be named "testInput" rather than the default "input".
     
///;


doc///
 Key
   NameParameterFile
   [writeParameterFile, NameParameterFile]
 Headline
   This option names the parameter file. 
 Description
   Text
     Set this option to a string to name the parameter file.
   Example
     writeParameterFile(storeBM2Files,{.1,.2,.5},NameParameterFile=>"testParameters")  --this function writes a parameter file named testParameters
     
///;


doc///
 Key
   SolutionFileStyle
   [b'PHSequence, SolutionFileStyle]
 Headline
   This is to adjust to the different ways Bertini stores solutions.
 Description
   Text      
     The option can be set to the "simple" style (coordinates only), "raw_solutions" style (path number and coordinates), and "main_data" style (detailed information).          

///;


doc///
 Key
   NameSolutionsFile
   [importSolutionsFile, NameSolutionsFile]
 Headline
   This option names the solution file which we import. 
 Description
   Text
     Set this option to a string to name the solution file. 
   Example
     makeB'InputFile(storeBM2Files,AVG=>{x},B'Polynomials=>{"x^2-2"})
     runBertini(storeBM2Files)
     importSolutionsFile(storeBM2Files,NameSolutionsFile=>"nonsingular_solutions")     
     importSolutionsFile(storeBM2Files,NameSolutionsFile=>"real_finite_solutions")     
     
///;


doc///
 Key
   InputFileDirectory
   [runBertini, InputFileDirectory]
 Headline
   This option is set to the directory where the Bertini input file is located. 
 Description
   Text
     Set this option to the directory where the Bertini input file is located.  
   Example
     R=QQ[x,y]
     theDir1 = temporaryFileName()
     makeDirectory theDir1
     theDir2 = temporaryFileName()
     makeDirectory theDir2
     makeB'InputFile(theDir1,
	 AVG=>{{x,y}},
	 B'Polynomials=>{x+y-1,x^2-2})
     runBertini(theDir2,InputFileDirectory=>theDir1)
     
///;

doc///
 Key
   StartFileDirectory
 Headline
   This option is set to the directory where the Bertini start file is located. 
 Description
   Text
     Set this option to the directory where the Bertini start file is located.  
     
///;

doc///
 Key
   StartParameterFileDirectory
 Headline
   This option is set to the directory where the Bertini start parameter file is located. 
 Description
   Text
     Set this option to the directory where the Bertini start parameter file is located.  
     
///;







doc///
 Key
   ConditionNumTol
   [bertiniZeroDimSolve, ConditionNumTol]
 Headline
   numerical tolerance for determining singular status   
 Description
   Text
     Endpoint is flagged as singular if multiple paths lead to it or condition number exceeds 
     @TO ConditionNumTol@. The default value of @TO ConditionNumTol@ is 1e10.
///;


doc ///
  Key
    "Bertini options"
    MPTYPE
    PRECISION
    ODEPREDICTOR
    TRACKTOLBEFOREEG
    TRACKTOLDURINGEG
    FINALTOL
    MAXNORM
    MINSTEPSIZEBEFOREEG
    MINSTEPSIZEDURINGEG
    IMAGTHRESHOLD
    COEFFBOUND
    DEGREEBOUND
    CONDNUMTHRESHOLD
    RANDOMSEED
    SINGVALZEROTOL
    ENDGAMENUM
    USEREGENERATION
    SECURITYLEVEL
    SCREENOUT
    OUTPUTLEVEL
    STEPSFORINCREASE
    MAXNEWTONITS
    MAXSTEPSIZE
    MAXNUMBERSTEPS
    MAXCYCLENUM
    REGENSTARTLEVEL
    [bertiniZeroDimSolve, MPTYPE]
    [bertiniZeroDimSolve, PRECISION]
    [bertiniZeroDimSolve, ODEPREDICTOR]
    [bertiniZeroDimSolve, TRACKTOLBEFOREEG]
    [bertiniZeroDimSolve, TRACKTOLDURINGEG]
    [bertiniZeroDimSolve, FINALTOL]
    [bertiniZeroDimSolve, MAXNORM]
    [bertiniZeroDimSolve, MINSTEPSIZEBEFOREEG]
    [bertiniZeroDimSolve, MINSTEPSIZEDURINGEG]
    [bertiniZeroDimSolve, IMAGTHRESHOLD]
    [bertiniZeroDimSolve, COEFFBOUND]
    [bertiniZeroDimSolve, DEGREEBOUND]
    [bertiniZeroDimSolve, CONDNUMTHRESHOLD]
    [bertiniZeroDimSolve, RANDOMSEED]
    [bertiniZeroDimSolve, SINGVALZEROTOL]
    [bertiniZeroDimSolve, ENDGAMENUM]
    [bertiniZeroDimSolve, USEREGENERATION]
    [bertiniZeroDimSolve, SECURITYLEVEL]
    [bertiniZeroDimSolve, SCREENOUT]
    [bertiniZeroDimSolve, OUTPUTLEVEL]
    [bertiniZeroDimSolve, STEPSFORINCREASE]
    [bertiniZeroDimSolve, MAXNEWTONITS]
    [bertiniZeroDimSolve, MAXSTEPSIZE]
    [bertiniZeroDimSolve, MAXNUMBERSTEPS]
    [bertiniZeroDimSolve, MAXCYCLENUM]
    [bertiniZeroDimSolve, REGENSTARTLEVEL]
    [bertiniComponentMemberTest, MPTYPE]
    [bertiniComponentMemberTest, PRECISION]
    [bertiniComponentMemberTest, ODEPREDICTOR]
    [bertiniComponentMemberTest, TRACKTOLBEFOREEG]
    [bertiniComponentMemberTest, TRACKTOLDURINGEG]
    [bertiniComponentMemberTest, FINALTOL]
    [bertiniComponentMemberTest, MAXNORM]
    [bertiniComponentMemberTest, MINSTEPSIZEBEFOREEG]
    [bertiniComponentMemberTest, MINSTEPSIZEDURINGEG]
    [bertiniComponentMemberTest, IMAGTHRESHOLD]
    [bertiniComponentMemberTest, COEFFBOUND]
    [bertiniComponentMemberTest, DEGREEBOUND]
    [bertiniComponentMemberTest, CONDNUMTHRESHOLD]
    [bertiniComponentMemberTest, RANDOMSEED]
    [bertiniComponentMemberTest, SINGVALZEROTOL]
    [bertiniComponentMemberTest, ENDGAMENUM]
    [bertiniComponentMemberTest, USEREGENERATION]
    [bertiniComponentMemberTest, SECURITYLEVEL]
    [bertiniComponentMemberTest, SCREENOUT]
    [bertiniComponentMemberTest, OUTPUTLEVEL]
    [bertiniComponentMemberTest, STEPSFORINCREASE]
    [bertiniComponentMemberTest, MAXNEWTONITS]
    [bertiniComponentMemberTest, MAXSTEPSIZE]
    [bertiniComponentMemberTest, MAXNUMBERSTEPS]
    [bertiniComponentMemberTest, MAXCYCLENUM]
    [bertiniComponentMemberTest, REGENSTARTLEVEL]
    [bertiniParameterHomotopy, MPTYPE]
    [bertiniParameterHomotopy, PRECISION]
    [bertiniParameterHomotopy, ODEPREDICTOR]
    [bertiniParameterHomotopy, TRACKTOLBEFOREEG]
    [bertiniParameterHomotopy, TRACKTOLDURINGEG]
    [bertiniParameterHomotopy, FINALTOL]
    [bertiniParameterHomotopy, MAXNORM]
    [bertiniParameterHomotopy, MINSTEPSIZEBEFOREEG]
    [bertiniParameterHomotopy, MINSTEPSIZEDURINGEG]
    [bertiniParameterHomotopy, IMAGTHRESHOLD]
    [bertiniParameterHomotopy, COEFFBOUND]
    [bertiniParameterHomotopy, DEGREEBOUND]
    [bertiniParameterHomotopy, CONDNUMTHRESHOLD]
    [bertiniParameterHomotopy, RANDOMSEED]
    [bertiniParameterHomotopy, SINGVALZEROTOL]
    [bertiniParameterHomotopy, ENDGAMENUM]
    [bertiniParameterHomotopy, USEREGENERATION]
    [bertiniParameterHomotopy, SECURITYLEVEL]
    [bertiniParameterHomotopy, SCREENOUT]
    [bertiniParameterHomotopy, OUTPUTLEVEL]
    [bertiniParameterHomotopy, STEPSFORINCREASE]
    [bertiniParameterHomotopy, MAXNEWTONITS]
    [bertiniParameterHomotopy, MAXSTEPSIZE]
    [bertiniParameterHomotopy, MAXNUMBERSTEPS]
    [bertiniParameterHomotopy, MAXCYCLENUM]
    [bertiniParameterHomotopy, REGENSTARTLEVEL]
    [bertiniPosDimSolve, MPTYPE]
    [bertiniPosDimSolve, PRECISION]
    [bertiniPosDimSolve, ODEPREDICTOR]
    [bertiniPosDimSolve, TRACKTOLBEFOREEG]
    [bertiniPosDimSolve, TRACKTOLDURINGEG]
    [bertiniPosDimSolve, FINALTOL]
    [bertiniPosDimSolve, MAXNORM]
    [bertiniPosDimSolve, MINSTEPSIZEBEFOREEG]
    [bertiniPosDimSolve, MINSTEPSIZEDURINGEG]
    [bertiniPosDimSolve, IMAGTHRESHOLD]
    [bertiniPosDimSolve, COEFFBOUND]
    [bertiniPosDimSolve, DEGREEBOUND]
    [bertiniPosDimSolve, CONDNUMTHRESHOLD]
    [bertiniPosDimSolve, RANDOMSEED]
    [bertiniPosDimSolve, SINGVALZEROTOL]
    [bertiniPosDimSolve, ENDGAMENUM]
    [bertiniPosDimSolve, USEREGENERATION]
    [bertiniPosDimSolve, SECURITYLEVEL]
    [bertiniPosDimSolve, SCREENOUT]
    [bertiniPosDimSolve, OUTPUTLEVEL]
    [bertiniPosDimSolve, STEPSFORINCREASE]
    [bertiniPosDimSolve, MAXNEWTONITS]
    [bertiniPosDimSolve, MAXSTEPSIZE]
    [bertiniPosDimSolve, MAXNUMBERSTEPS]
    [bertiniPosDimSolve, MAXCYCLENUM]
    [bertiniPosDimSolve, REGENSTARTLEVEL]
    [bertiniRefineSols, MPTYPE]
    [bertiniRefineSols, PRECISION]
    [bertiniRefineSols, ODEPREDICTOR]
    [bertiniRefineSols, TRACKTOLBEFOREEG]
    [bertiniRefineSols, TRACKTOLDURINGEG]
    [bertiniRefineSols, FINALTOL]
    [bertiniRefineSols, MAXNORM]
    [bertiniRefineSols, MINSTEPSIZEBEFOREEG]
    [bertiniRefineSols, MINSTEPSIZEDURINGEG]
    [bertiniRefineSols, IMAGTHRESHOLD]
    [bertiniRefineSols, COEFFBOUND]
    [bertiniRefineSols, DEGREEBOUND]
    [bertiniRefineSols, CONDNUMTHRESHOLD]
    [bertiniRefineSols, RANDOMSEED]
    [bertiniRefineSols, SINGVALZEROTOL]
    [bertiniRefineSols, ENDGAMENUM]
    [bertiniRefineSols, USEREGENERATION]
    [bertiniRefineSols, SECURITYLEVEL]
    [bertiniRefineSols, SCREENOUT]
    [bertiniRefineSols, OUTPUTLEVEL]
    [bertiniRefineSols, STEPSFORINCREASE]
    [bertiniRefineSols, MAXNEWTONITS]
    [bertiniRefineSols, MAXSTEPSIZE]
    [bertiniRefineSols, MAXNUMBERSTEPS]
    [bertiniRefineSols, MAXCYCLENUM]
    [bertiniRefineSols, REGENSTARTLEVEL]
    [bertiniSample, MPTYPE]
    [bertiniSample, PRECISION]
    [bertiniSample, ODEPREDICTOR]
    [bertiniSample, TRACKTOLBEFOREEG]
    [bertiniSample, TRACKTOLDURINGEG]
    [bertiniSample, FINALTOL]
    [bertiniSample, MAXNORM]
    [bertiniSample, MINSTEPSIZEBEFOREEG]
    [bertiniSample, MINSTEPSIZEDURINGEG]
    [bertiniSample, IMAGTHRESHOLD]
    [bertiniSample, COEFFBOUND]
    [bertiniSample, DEGREEBOUND]
    [bertiniSample, CONDNUMTHRESHOLD]
    [bertiniSample, RANDOMSEED]
    [bertiniSample, SINGVALZEROTOL]
    [bertiniSample, ENDGAMENUM]
    [bertiniSample, USEREGENERATION]
    [bertiniSample, SECURITYLEVEL]
    [bertiniSample, SCREENOUT]
    [bertiniSample, OUTPUTLEVEL]
    [bertiniSample, STEPSFORINCREASE]
    [bertiniSample, MAXNEWTONITS]
    [bertiniSample, MAXSTEPSIZE]
    [bertiniSample, MAXNUMBERSTEPS]
    [bertiniSample, MAXCYCLENUM]
    [bertiniSample, REGENSTARTLEVEL]
    [bertiniTrackHomotopy, MPTYPE]
    [bertiniTrackHomotopy, PRECISION]
    [bertiniTrackHomotopy, ODEPREDICTOR]
    [bertiniTrackHomotopy, TRACKTOLBEFOREEG]
    [bertiniTrackHomotopy, TRACKTOLDURINGEG]
    [bertiniTrackHomotopy, FINALTOL]
    [bertiniTrackHomotopy, MAXNORM]
    [bertiniTrackHomotopy, MINSTEPSIZEBEFOREEG]
    [bertiniTrackHomotopy, MINSTEPSIZEDURINGEG]
    [bertiniTrackHomotopy, IMAGTHRESHOLD]
    [bertiniTrackHomotopy, COEFFBOUND]
    [bertiniTrackHomotopy, DEGREEBOUND]
    [bertiniTrackHomotopy, CONDNUMTHRESHOLD]
    [bertiniTrackHomotopy, RANDOMSEED]
    [bertiniTrackHomotopy, SINGVALZEROTOL]
    [bertiniTrackHomotopy, ENDGAMENUM]
    [bertiniTrackHomotopy, USEREGENERATION]
    [bertiniTrackHomotopy, SECURITYLEVEL]
    [bertiniTrackHomotopy, SCREENOUT]
    [bertiniTrackHomotopy, OUTPUTLEVEL]
    [bertiniTrackHomotopy, STEPSFORINCREASE]
    [bertiniTrackHomotopy, MAXNEWTONITS]
    [bertiniTrackHomotopy, MAXSTEPSIZE]
    [bertiniTrackHomotopy, MAXNUMBERSTEPS]
    [bertiniTrackHomotopy, MAXCYCLENUM]
    [bertiniTrackHomotopy, REGENSTARTLEVEL]
  Headline
    options for methods of Bertini package
  Description
    Text
      Every function of the package takes ALL optional arguments listed here.
      The default value for EACH option is -1, which tells Bertini to use its internal default.
      Refer to Appendix E of SIAM Bertini book for full details and list of options. 

      MPTYPE: Type of precision (0=double, 1=fixed higher, 2=adaptive).

      PRECISION: Precision, in bits, when used MPTYPE=1.

      ODEPREDICTOR: Choice of predictor method (9 choices).

      TRACKTOLBEFOREEG: Before endgame zone, Newton error must be less than this for success. 

      TRACKTOLDURINGEG: Same as previous, but during endgame.

      FINALTOL: Path is deemed successful if final two endpoint approximations agree to FINALTOL.

      MAXNORM: If SECURITYLEVEL=0, path is truncated if two consecutive endpoint approximations exceed this value. 

      MINSTEPSIZEBEFOREEG: Path is truncated if stepsize drops below this level before endgame.

      MINSTEPSIZEDURINGEG: Same as previous, but during endgame.

      IMAGTHRESHOLD: Endpoint deemed real if infinity norm is smaller than this. 

      COEFFBOUND: Useful only if MPTYPE=2, bound on sum of coefficients of each polynomial. 

      DEGREEBOUND: Useful only if MPTYPE=2, bound on degree of each polynomial.

      CONDNUMTHRESHOLD: Endpoint is deemed singular if multiple paths lead to it or condition number exceeds this. 

      RANDOMSEED: Useful to repeat runs with the same random numbers.

      SINGVALZEROTOL: Singular value is considered 0 if less than this value, when using fixed precision.

      ENDGAMENUM: Choice of endgame (1=power series, 2=Cauchy, 3=trackback Cauchy).

      USEREGENERATION: 1 to use regeneration for a zero-dimensional run.

      SECURITYLEVEL: 1 to avoid truncation of possibly-infinite paths.

      SCREENOUT: Level of output to the screen.

      OUTPUTLEVEL: Level of output to files.

      STEPSFORINCREASE: Number of consecutive Newton corrector successes before increase of stepsize.

      MAXNEWTONITS: Newton corrector step deemed failed if no convergence prior to this number of iterations. 

      MAXSTEPSIZE: Largest stepsize allowed. 

      MAXNUMBERSTEPS: Max number of steps for entire path.  Path failure if number of steps exceeds this.

      MAXCYCLENUM: Max cycle number considered during endgame.

      REGENSTARTLEVEL: Level at which regeneration begins. 

      There are two recommended ways of using the optional arguments.
    
      (1) Specify individual parameters in a function call:
    Example
      CC[x,y]; F = {x^2-1,y^2-1};
      bertiniZeroDimSolve(F,RANDOMSEED=>0,TRACKTOLBEFOREEG=>1e-6,FINALTOL=>1e-100)
    Text
      (2) Store your frequently used favorites in an OptionTable
      and pass it as the last argument in each function call:
    Example
      opts = new OptionTable from {RANDOMSEED=>0,TRACKTOLBEFOREEG=>1e-6,FINALTOL=>1e-100}
      bertiniZeroDimSolve(F,opts)
      G = {x^2+y^2-1};
      bertiniPosDimSolve(G,opts)
///;

end


-- to be added in another version
doc ///
 Key
   bertiniSegmentHomotopy
   (bertiniSegmentHomotopy,List, List, List)
 Headline
   constructs and tracks a straight-line homotopy
 Usage
   S0 = bertiniSegmentHomotopy()
 Inputs
   start:List
     start system, list of polynomial equations
   tar:List
     target system, list of polynomial equations
   S1:List
     start solutions (solutions of start system when t=1)
 Outputs
   S0:List
     target solutions (solutions of target system when t=0)
 Description
   Text
     Constructs and tracks a straight line homotopy from the start system to the target system.
   Example
     R=CC[x,y]
     start={x^3-1, y^3-1}
     tar={x^3+y^3-4,x^3+2*y-1}
     --input start system, target system, list of start points
     bertiniSegmentHomotopy(
     start,tar,{{1,1},{-.5-0.86603*ii,1},{1,-0.5+0.86603*ii}})
///;

doc ///
 Key
   importPoints   
   (importPoints,String,ZZ)
 Headline
   importPoints reads solutions from a Bertini solution file to store as points in M2
 Usage
   S=importPoints(l,n) 
 Inputs
   l: String
     A string giving the  locaton of a Bertini solution file.
   n: ZZ
     Number of coordinates for each solution.
 Outputs
   S: List
     of solutions of type Point
 Description
   Text
     This method imports points from a solution file that Bertini created while solving 
     a zero-dimensional system.
     The string l is a path which gives the location of the solution file and n is
     an integer stating the desired number of coordinates for each solution. 
     When solving a zero-dimensional
     system, Bertini creates several solution files; @TO importPoints@ works 
     with the following Bertini solution files: "finite_solutions", "nonsingluar-solutions",
     "real_finite_solutions", "singluar_solutions". 
--     The output is a list of points.
     The user can specify which solutions to read from the file using the @TO SpecifyPoints@ option
     or which coordinates to select using the @TO SpecifyCoordinates@ option.
     
--   Example
     --locationOfSolutionFile="/Users/.../YourFolder/solution_file";
--     A=importPoints(locationOfSolutionFile,4)
     --The output would be a list of points that have 4 coordinates.          
--   Example 
--     locationOfSolutionFile="/Users/.../YourFolder/solution_file";
--     B=importPoints(locationOfSolutionFile,4,SpecifyPoints=>{0,2})
     --The output would be the first and third solutions of the file. 
--   Example 
--     locationOfSolutionFile="/Users/.../YourFolder/solution_file";
--     C=importPoints(locationOfSolutionFile,4,SpecifyCoordinates=>{0,1})
     --The output would be the first and second coordinate of each solution of the file.  
 --Caveat
   --The method importPoints will not
   --For importPoints to be successful, the Bertini solution file must have a particular format.

   --The first line is an integer, the number of solutions in the.
   --The next lines consist of a blank line followed by a line for each coordinate;
   --these lines consist of: RR|"e"|ZZ" "RR|"e"|ZZ for scientific notation of the real and imaginary parts of the coordinate.
///;

doc ///
 Key
   phPostProcess
   (phPostProcess,String,List,ZZ)
 Headline
   Does post processing parameter homotopy.
 Usage
   S=phPostProcess(sIn,L,n) 
 Inputs
   sIn: String
     A string giving the directory of the input files.
   L: List
     A list of parameters. 
   n: ZZ
     Number of coordinates in a solution.
 Description
   Text
     The purpose of this function is to allow a person
     to share their Bertini computations with a second user,
     who can then analyze the data easily with the Bertini.M2 interface.   
     
     Instead of doing a parameter run by calling Bertini, 
     the PrintNotes option prints a file titled "notes"  located in the input file's directory.
     If the "notes" file does not exist it returns an error.   
     
     The output will be a list of points that have 3 coordinates, that are solutions to a parameterized system of equations evaluated at L, found by doing a parameter homotopy. 
--   Example
--     inputFileLocation="/Users/.../YourFolderA";
--     L={.8234+ii*8,9}--A list of two parameter values.
--     n=3--A solution has n coordinates.
--     phPostProcess(inputFileLocation,L,n)     
--   Example
--     inputFileLocation="/Users/.../YourFolderA";
--     phPostProcess(inputFileLocation,"",{},0,PrintNotes=>1)
 Caveat
   Even if Bertini is called but does not run,  
   an error may not be reported if previous solution files were already in the outputDirectory.
///;


doc ///
 Key
   phMonodromy
   (phMonodromy,String,ZZ,ZZ)
 Headline
   Does a sequence of parameter homotopies.
 Usage
   S=phMonodromy(sIn,p,n) 
 Inputs
   sIn: String
     A string giving the directory of start files: input, start, start_parameters
   p: ZZ
     Number of parameters.
   n: ZZ
     Number of coordinates of a point.
         
///;
--ref{} need to add about the option ParameterValues

doc ///
 Key
   SpecifyPoints
   [importPoints, SpecifyPoints]
 Headline
   optional argument to specify which solutions to import
 Usage
    importPoints(...,SpecifyPoints=>List)
              
///;

doc///
 Key
   AllowStrings
   [bertiniTrackHomotopy, AllowStrings]
   [bertiniParameterHomotopy, AllowStrings]
   [bertiniPosDimSolve, AllowStrings]
   [bertiniZeroDimSolve, AllowStrings]
 Headline
   input a system of polynomials as strings   
 Description
   Text
     Optional input that takes a List of variables and allows for the polynomial system to
     be passed as a List of strings to @TO bertiniZeroDimSolve@, @TO bertiniTrackHomotopy@,
     @TO bertiniParameterHomotopy@, and @TO bertiniPosDimSolve@.
   Example
     R = CC[x,y,z];
     f = {"(x^2+y^2-z^2)*(z-x)",toString (hold (x^2+y^2-z^2)*(z+y)), z-1};
     sols = bertiniZeroDimSolve(f, AllowStrings=>{x,y,z})
   Example 
     R=CC[x,y,z];--u1,u2 are parameters
     f1=x^2+y^2-z^2;
     f2="u1*x+u2*y";
     f3=z-1;
     finalParameters={{0,1}};
     bPH=bertiniParameterHomotopy( {f1,f2,f3}, {u1,u2},{finalParameters },AllowStrings=>{x,y,z})            
   Example 
     R=CC[x,t1];
     f1="x^2+cos(1-t1)-2*exp(1-t1)";
     H={f1};
     sol1 = point {{1}};
     sol2 = point {{-1}};
     S1={sol1,sol2}--solutions to H when t=1                 
     S0 = bertiniTrackHomotopy (t1, H, S1,AllowStrings=>{x}) --solutions to H when t=0|
     peek S0
///;

doc///
 Key
   SubFunctions
   [bertiniTrackHomotopy, SubFunctions]
   [bertiniParameterHomotopy, SubFunctions]
   [bertiniPosDimSolve, SubFunctions]
   [bertiniZeroDimSolve, SubFunctions]
 Headline
   optional argument to specify subfunctions to be written to the Bertini input file   
 Description
   Text
     The option is a list of pairs that define a subfunction. 
   Example
     R = CC[x,y,z][s1,s2];
     sF={ {s1,(x^2+y^2-z^2)},{s2,z-x}}--s1=x^2+y^2-z^2; s2=z-x;
     f = {s1*s2, s1*(z+y),z-1};
     sols = bertiniZeroDimSolve(f, AllowStrings=>{x,y,z}, SubFunctions=>sF)
///;