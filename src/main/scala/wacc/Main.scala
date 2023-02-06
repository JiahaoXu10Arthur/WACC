package wacc

import parsley.{Success, Failure}
import java.nio.file.Files
import java.nio.file.Paths
import SemanticErrorBuilder._

object Main {
	def main(args: Array[String]): Unit = {
		// val filename = args.head
		// val string = new String(Files.readAllBytes(Paths.get(filename)))



		// Parser.parse(string) match {
		// 	case Success(x) => {
		// 		println(s"${args.head} parse success: ")
		// 		println(x)
		// 	//  Comment out for stopping sbt shutting down	
		// 	//	System.exit(0)
		// 	}
		// 	case Failure(msg)	=> {
		// 		println(s"${args.head} parse fail: ")
		// 		println(msg)
		// 	//	System.exit(100)
		// 	}
		// }

		println(SemanticErrorBuilder.buildWithMsg(None, (0, 0), Seq("hello", "world"), "line info"))
	}
}

