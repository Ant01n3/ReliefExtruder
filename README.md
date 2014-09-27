# ReliefExtruder

A height-map generator that produce files in STL suitable for 3D printing. The height maps are grids of regularly spaced points describing the elevation or height of, for example, a geographical map. The program reads these points and generated a 3D model of it in the STL format, suitable for use with many 3D printers and 3D softwares.

## Compilation

Using sbt, simply enter:

    compile

## Launching

### With an executable jar

Using sbt, you can issue the command:

    assembly

This will generate an executable jar named `ReliefExtruder.jar` in the `target/scala-2.11` directory. The jar contains all the dependancies. Simply execute it:

    java -jar ReliefExtruder.jar

This will print an usage description.

### From the sources

Using sbt, you can issue the command:

    add-start-script-tasks

Then, the following command will create a script in `target` that can start the application:

    startScript

Go to the `target` directory and enter:

    ./start org.litis.relief.ReliefExtruder

This line will print an usage description.

## Usage

The extruder works with specialy prepared `.csv` files or specially prepared `.png` images. The input files describe a grid of points. Each point defines a value as the height of the terrain 

### Input format CSV

CSV files are plain text files where values are separated by a delimiter, often a comma (hence the name CSV: Comma Separated Values). Here the values must be separated by `;`` to avoid confusing the comma inside numbers (in some European countries, the dot is indicated as a comma).

Here the file must include a header that must contain, in this order lines describing the data, first a `ncols`
and `nrows` indicators that give the dimension of the area:

    ncols;1000;;;
    nrows;1000;;;

The number of `;` at the end of the line is not important, most of the time the CSV comes from a spreadsheet software that will put as many `;` after `ncols` than there are columns in the spreadsheet.

Then, an indicator of the distance in meters between each point of data and a special value indicating that some points do no represent data (for partially described maps):

    cellsize;1;;;
    NODATA_value;-9999;;

After this header simply comes the data, there are as many lines as `nrows` indicates and each line is made of as many real numbers (possibly negative) than `ncols` all separated by `;`. The values are assumed to be meters.

### Input format PNG

PNG file can also be used, they must represent a map where color indicates height. The range of color follows the chromatic cirle, often seen in HSV color models. This also follows the light spectrum: Red, Yellow, Green, Cyan, Blue, Magenta. The value and saturation of the color is not used, but you should probably use plainly saturated colors with the value to 1.

TODO put an example.

### Options

The basic usage is to call the program with as argument a file in CSV or PNG format. This will produce on the standard output an ASCII STL file

    java -jar ReliefExtruder.jar myFile.png

ASCII STL files can become very large. To create a more compact binary STL into a file, use the `-out` option:

    java -jar ReliefExtruder.jar myfile.png -out myfile.stl

This usage will only produce a *surface* 3D file. This means that only the points described by the input file are transformed into 3D triangles representing the surface like a crumpled sheet of paper. To create a volume as if the resulting surface was extruded from a block of terrain, use the `-volume` option. This option takes a parameter that give the height in meters of the volume in addition the the max height of the heigh map. The final height will then be the difference between the lowest and highest values of the data plus the volume base height.

The 3D object produced uses the `cellsize` parameter of CSV or assume one meter for PNG. You can scale the whole output model using the `-scale` option. For example using `0.1` will divide by `10` the size of the output model. You can also play only on the elevation with `-yscale`. This options will only scale the elevation of the values. These two parameters can be used at the same time, they multiply.

You can also ask to produce a model that is a part only of the input data. Use the `-box` option with four parameters: the start along X (columns) the end along X, the start along Y (rows) and the end along Y. The values here are indices for rows and columns. They start at zero.

When using an image as input, the file does not specify how the color map to values. We use a chromatic circle where the first and highest value is red, and the last and lowest value is magenta. The `-imagescale` option allows to map values to these colors, other values being linearly interpolated. You give two paramters, first the min value, mapping to magenta, then the max value mapping to red. The values given here are assumed to be meters.

The `-cellsize` option allows to bypass the `cellsize` indicator in CSV and to replace the default value of 1 in PNG images. It specifies the spacing between data points in meters.

## Limitations

This is a work in progress, tested with few input files... help is appreciated ;-)
