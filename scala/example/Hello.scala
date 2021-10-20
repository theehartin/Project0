package example

object Hello {
    def main(args: Array[String]){

      val rt = scala.io.StdIn.readLine("Please enter a number ")
      println(rt)

      
      val breedName: String =  scala.io.StdIn.readLine("Please enter Breed Name: ")

      print(s"Please enter $breedName minimum height: ")
      val minHeight: Float =  scala.io.StdIn.readFloat()


    }

}


