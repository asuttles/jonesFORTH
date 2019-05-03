# jonesFORTH

This is my own port and extension of Richard Jones' Minimal FORTH compiler.

The literate source code, writen in x86 assembly (gas syntax), may be found at RMJ's git repository here: [jonesforth.git](http://git.annexia.org/?p=jonesforth.git;a=summary).

The original announcement for the project may be found here: [Lambda the Ultimate](http://lambda-the-ultimate.org/node/2452).

My own port of the compiler converts the code to 32-bit intel syntax, which may be assembled by the Microsoft masm assembler that is included as a part of the masm32 project.  The project relies heavily on masm macros and extensions provided by the masm32 project.

The code is hosted here so that I don't lose track of it and so that it might inspire me one day to port the code to 64-bit nasm syntax and continue extending the language features.

## Getting Started

The best way to learn about Jones' indirect threaded FORTH interpreter is to read the original source.

In order to compile my port, the masm32 SDK must be installed from: [masm32 SDK](www.masm32.com).

The interpreter is assembled using the included makefile.

A .forth is included in this distribution in order to build the rest of the FORTH dictionary and extend the languge.

## Contributing

Contributions, comments, suggestions are welcome.

## Authors

* **Richard MW Jones** - *Creator* - [rwmj](https://rwmj.wordpress.com/)
* **Andrew Suttles** - *masm32 port* - [acs](https://github.com/asuttles)

## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details

## Acknowledgments

* Many thanks to Richard Russell whose own extensions and modifications (especially DO-LOOPs)  to the original source code have been helpful to me.

