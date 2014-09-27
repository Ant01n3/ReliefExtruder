# ReliefExtruder

A height-map generator that produce files in STL suitable for 3D printing.

## Compilation

Using sbt, simply enter:

    compile

## Launching

### With an executable jar

Using sbt, you can issue the command:

    assembly

This will generated an executable jar named `ReliefExtruder.jar` in the `target/scala-2.11` directory. The jar contains all the dependancies. Simply execute it:

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

The extruder works with specialy prepared `.csv` files or specially prepared `.png` images.

TODO describe the input formats.

TODO describe the options.

## Limitations

This is a work in progress, tested with few input files... help is appreciated ;-)
