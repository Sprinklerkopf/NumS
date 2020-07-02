# NumS
Math and visualization toolkit for Scala

**STILL UNDER CONSTRUCTION**

This Scala library has the aim to extend Scala's (and Java's) native math, data processing and data visualization capabilities to make it easy to work on scientific tasks with the programming language family of Java. Mainly it's purpose is to solve, visualize and automate my math and algorithm homeworks :)

## Usage
This project is built with SBT so you can simply built it with the given build file (nums/build.sbt).
If you want to use it as a library delete the example usage File (Main.scala) and deploy it with sbt-assembly (https://github.com/sbt/sbt-assembly), the dependency are already added in project/plugins.sbt.

Especially Graph is heavily unfunctional. Suggestions to improve it are welcome.

## Contribute
Open problems im working on are:
* Two Visualization (currently only Graph.scala) options: JFrame (-> improve performance) and OpenGL (-> yet to implement with LWJGL3) 
* More methods for Vector and Matrix class
* More classes (ideas?)
  * Complex Numbers
    * Quaternions
* More Documentation (Java Doc)
Feel free to contribute and improve this project :)
