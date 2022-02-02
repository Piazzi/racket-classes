<div align="center">
  <h1> CLASSES LANGUAGE  </h1>
</div>

CLASSES is an object-oriented language. A program in the language consists of a sequence
of class declarations followed by an expression to be executed, which can make use of the classes
declared. A class declaration has a name immediately followed by the superclass name, contains
zero or more field declarations and zero or more method definitions. A statement of a method
contains a name, a list of formal parameters and a body.
The language presents four expressions to manipulate classes and objects. The expression new creates a
object of that class, from the execution of the initialize method of the class with the respective arguments
of the expression. The expression self returns the object on which the current method is operating. The expression
send consists of an expression that must evaluate to an object, a method and a sequence of zeros
or more operands. The method to be executed is obtained from the object's class and then executed with the
arguments referring to the evaluation of the operands. Finally, a super expression has the effect of executing a
method from the current object's class hierarchy, fetching the method in question from the superclass
of the object.

