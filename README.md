# FRACOR
Autolisp software toolbox for mapping fracture corridors on Autocad platform
The FRACOR package comes as a collection of 4 folders
EXAMPLE FIELD
EXP
INP
VLX
These are briefly explained below.

A.1 Application routines

VLX contains the compiled executable FRACOR.VLX toolbox which consists of the routines that are listed and briefly described below. The application routines can be subdivided into three groups: (i) fundamental modules, (ii) fracture data input ad plot routines, (iii) data export routines and (iv) a routine to locate wells.

A1.1 Fundamental modules

The fundamental set consists of the following modules:
DRAWCON: prepares structural contour map of reservoir top.
PLOTPTS: imports and plots all horizontal and vertical well spud locations.
WELTRACE: imports and plots horizontal well trajectories.
FAULTRACE & SPLINES: import faults as polylines or splines. These routines can also be used to trace boundaries, contours or any other feature that can be represented as polyline or spline.

A.1.2 Modules to input and plot well based FC data

Well based FC data usually comes as: well id; measured depth; and the data itself. In case of horizontal or deviated wells, the measured depth is the depth at which FC is observed on borehole images or anomalies observed on open-hole logs or flow meters etc. In case of vertical wells, measured depth is not needed but is set to zero for consistency with horizontal wells.
It is necessary to generate files with UTM coordinates to enable plotting. Therefore the FC data input and plotting routines come in pairs; one to convert well no and measured depth to coordinates, and the second to plot the FC data as sticks, spots or bubbles. If the initial data already has UTM coordinates the conversion routines may be skipped. 
FCs from borehole image logs have dip and strike and represented as sticks. The rest of the data are points as the orientation of the FCs is not known. The conversion and plotting modules for FCs from borehole images are: 
FINDSTICKS: to find and output geographic coordinates from well number and measured depth for FCs sticks from borehole images.
PLOTSTICKS: to plot the FC sticks on a map using the geographic coordinates, strike and length.
The conversion and plotting modules for spots or bubble data are:
FINDPTS: to find and output geographic coordinates from well number and measured depth of point data.
PLOTPTS: to plot points on a map.  The PLOTPTS routine can plot points as circles or polygons of specified number of corners, color and size with or without labels. As such PLOTPTS can be used not only to display well locations but also any kind of spot data like circulation losses. PLOTPTS can also be used to generate bubble plots of various static and dynamic data such as fracture density, productivity Index (PI), KH ratio, Injection and production rate, oil column thickness etc.

A.1.3 Output routines

FC on the final map are represented usually as sticks or polylines. The final map may also contain various relevant boundaries such as areas where no FCs are present or flood fronts, exclusion zones around horizontal wells. Boundaries are usually drawn as splines. It is therefore sufficient to have three output routines 
LINECEK: to export lines.
POLYCEK: to export polylines
SPLINECEK: to export splines.
These modules output lines, polylines or splines as text files with appropriate format. It is important to remember that SPLINECEK will recognize only splines that were input or originally drawn as splines. Polylines converted to splines are not recognized as splines. These three modules are designed to recognize and export lines, polylines or splines on a selected layer. The modules ask for an entity on the desired layer to recognize the layer.

A.1.4 finding wells

If there are too many wells, it may become difficult to find a well. An auxiliary module FINDWELL is included for finding wells. The routine inserts an arrow pointing to the well that is to be located. The arrow is included in the example data folder as an AutoCAD block, which is included as ARROW drawing file. One has to insert the arrow in the ARROW file first. Once the arrow is inserted as a block, every time a FINDWELL is run, it asks the well id and places the arrow block at the well spud location. FINDWELL searches the text file with all spud locations (ALLWELLOC.PRN file in the example data set).

A.2 Parameter files

Each module must be accompanied by a text input file with the name of the module and suffix. The .INP file contains various parameters that must be specified to run the respective module. For example the WELLTRACE.INP file must contain: (i) input data file name, (ii) map scale, (iii) UTM coordinates of the lower left and upper right corners of the map and few other parameters.  Both the .INP and data files must be prepared following the templates provided in the explanation files .EXP before running the module. 

A.3 Data text files

The format of each input data file is described and an example is provided in the .EXP file. For example, the input data file for WELLTRACE must have X coordinate, Y coordinate, measured depth and well number. X and Y coordinates are usually expressed as UTMs. Measured depth is in m or ft. It is important to note that the unit of measured depth in all other data files must be the same as the well trajectory input data. The input files may be exported from Excel as space delimited text files with suffix .PRN. In any case all files must be space delimited and not tab delimited.

A.4 Loading and Running the FRACOR library

One may to follow the steps below in order to load and use the FRACOR toolbox.

A.4.1 Initial Run

1 Create a new folder, say TEST
2 Copy the following into this folder
•	FRACOR.VLX and FRACOR.PRV
•	.INP files to be used
•	ARROW.dwg AutoCAD file
•	Data files (.PRN files in the example application). 
Data files must be prepared beforehand as text files. The name of the data file(s) must be specified along with the suffix in the first line of the .INP file.  Data may not be available for all application is the FRACOR library. Only the .INP files for intended application needs to be copied to the new folder.
3 Open AutoCAD, create new file. For example, TEST_FIELD.dwg
4 Save this file (TEST_FIELD.dwg) into the newly created folder (TEST).
5. Close the file TEST_FIELD.dwg  and reopen it from the TEST folder.
(steps 4 and 5 are needed to set the operating folder for AutoCAD. It is also possible to set the operating folder by setting the start in folder in properties window).
6 Load the FRACOR library by issuing the following LISP command
(LOAD”FRACOR”) or (load”fracor.vlx”)
The library is now ready to use. It will fetch all the .INP and DATA files from the same folder where the newly created folder (TEST folder in this example). As a last step, insert ARROW.DWG into the TESTFIELD drawing file at the origin. 

A.4.2 Subsequent runs

Once you create and save the AutoCAD application file (TEST_FIELD.dwg in this example), next time it is sufficient to double click and open the application file and load the FRACOR.VLX library.The usual procedure is to generate a different layer for each component of the FC data. The way to do it is to create a layer and copy the plot at layer 0 to this layer. A plot is always created at the active layer. One can ensure plots are initially placed at layer 0, by keeping layer zero as the active layer. It is also possible to activate the newly created layer. This way, the plot is directly placed on the intended layer.
Suppose a contour map is created by the DRAWCON routine. Just click the layer window and create a new layer and call it CONTOUR_MAP. Next, select the contour map plot and change layer to the newly defined CONTOUR_MAP layer.  Once a layer is created it can be frozen or turned off and hidden from view.
The FINDWELL module may be run to find any well which is included in the file that contains spud location of all drilled wells.
All the modules are run by typing the name of the module in parenthesis. For example, 
>(DRAWCON)
or
>(FAULTRACE)
The only exception is the FINDWELL routine. It is issued without parenthesis around as follows
>FW

A.5 Exporting lines, polylines and splines

The routines LINECEK, POLYCEK and SPLINECEK extract all lines or polylines on a layer. One needs to follow the steps below to extract lines or polylines.
1 Turn on and unlock the layer which contains the lines or polylines to be extracted. 
2 Open the LINECEK.INP (or POLYCEK.INP or SPLINECEK.INP) file and set the file names, scale and coordinates of origin.
3 Run LINECEK (or POLYCEK or SPLINECEK). The program asks to select an item on the desired layer. Click on any item on that layer. The program will extract all lines (or polylines, or splines) that exist on the selected layer and outputs them as a text file. 



A.6 Test Field data

A toy data set is included for a test field as a reference and guide. In addition to the FRACOR.VLX toolbox, and ARROW.DWG drawing and .INP files, the EXAMPLE_FIELD folder contains complete set of data to demonstrate initiating and using the toolbox. 
