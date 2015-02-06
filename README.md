### random-bag

A `Bag` is a simple data structure with O(1) insert, delete, random
read and random modify. It's essentially a wrapped up vector with a RNG.

This can be useful for things like Monte Carlo simulations where you
need to select a random value from a set and don't care about lookup.

