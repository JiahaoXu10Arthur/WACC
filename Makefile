all:
	sbt compile assembly

clean:
	sbt clean && rm -rf wacc-17-compiler.jar

.PHONY: all clean
