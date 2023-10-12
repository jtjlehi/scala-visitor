# Visitor Pattern Variants in Scala

This was a couple of ideas I had on how to implement something resembling the visitor pattern in Scala using some of the tools Scala has that other Object Oriented languages may not have.

My first Idea was to use Type Classes. I didn't know much about type classes so I coded a few examples to get a better feel for them. It turns out that type classes don't work for the visitor pattern because accept needs to be dynamically dispatched.

The second idea I had was to use PartialFunctions. This turned out to be extremely fruitful. The idea was to have a partial function passed as a parameter to a function that constructs an anonymous instance of the Visitor class. This approach has many benifits, including the nice feature that creating what is traditionally inherited behavior is just a matter of function composition.

