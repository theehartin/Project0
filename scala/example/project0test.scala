package example

import org.mongodb.scala._
import scala.io.Source
import org.mongodb.scala.model.Filters._
import org.mongodb.scala.model.Projections._
import org.mongodb.scala.model.Sorts._
import org.mongodb.scala.model.Aggregates._
import org.mongodb.scala.model.Updates._
//import scala.util.parsing.json._
//import org.mongodb.scala.model.Indexes._

//import org.mongodb.scala.bson._

import example.Helpers._
import java.io.FileNotFoundException



object MongoDemo {
  def main(args: Array[String]) {
    println("Starting MongoDB - Scala Demo...")


//Global Variables/////////////////////////////////////////
    val client: MongoClient = MongoClient()
    var database: MongoDatabase = client.getDatabase("Project0")
    var collection: MongoCollection[Document] = database.getCollection("dogBreedData")
    var filterCollection = collection






    //Miscellaneous functions//////////////////////////////////

    //Add 3 line Breaks
   def space() {
      println()
      println()
      println()
    }

    //Creating a temporary collection
    def tempCollectionCreation(query: Seq[Document]){
      //Initialize the temporary collection variable
      lazy val tempCollection: MongoCollection[Document] = database.getCollection("tempCollection")

      //If the search has been refined before the current iteration, a tempCollection
      //has already been created and therefore needs to be dropped lest we duplicate 
      //collections and documents, ultimately running into an error
      tempCollection.drop().results

      //Creating the tempCollection
      for (doc <- query){
        tempCollection.insertOne(doc).results()
      } 

      //Print's the number of documents passed to the temporary collection
      space()  
      println("Number of Breeds: ")      
      tempCollection.count().printResults()
      space()

      //Sets the filter function to the newly created tempCollection to allow this function
      //to be ran recursively
      filterCollection = tempCollection 
      filterDirectory()
    }

  def refineFunction(query: Seq[Document]){
    println()
    val b: String = scala.io.StdIn.readLine("Would you like to filter the results?(yes or no): ").toLowerCase()
    b match{
      case "yes" => tempCollectionCreation(query)      
      case "no"  => directory()
      case _ => {
      println("I'm sorry, that's not an option")
      refineFunction(query)
      }   
    }
  }







 //Admin functions//////////////////////////////////////////

    
    def showDatabases() {
      println("Databases:")
      client.listDatabaseNames().printResults()
      space()
      directory()
    }

    //Drop a database
    def dropDatabase(){
      val a: String = scala.io.StdIn.readLine("Which Database would you like to drop?: ")
      client.getDatabase(a).drop().headResult()
      println(s"$a has been dropped")  
      directory()
    }
    
    //Change Databases
    def useDatabase(){
      println()
      val a: String = scala.io.StdIn.readLine("Which Database would you like to use?: ")
      database = client.getDatabase(a)
      println(s"You are now using the $a Database")  


      directory()

    }

    //Upload a File from the computer
    def readFile() = { 

      def jsonReading(){

        showCollectionsInternal()
        val b: String = scala.io.StdIn.readLine("Which Collection would you like to use?: ")
        collection = database.getCollection(s"$b")

        def jsonRecurse(){
          val c: String = scala.io.StdIn.readLine("Which JSON File would you like to Use?\n" +
            "Please provide full File Path: ")
          
          try{  
            val stringDocuments = Source
              .fromFile(c)
              .getLines
              .toList
            val bsonDocuments = stringDocuments.map(doc => Document(doc))
            collection.insertMany(bsonDocuments).printResults()
            filterCollection = collection
          } catch{
            case ex: FileNotFoundException => {
              println()
              println("File Not Found")
              val a : String = scala.io.StdIn.readLine("Would you like to try again? (yes or no): ").toLowerCase()
              a match {
                case "yes" => jsonRecurse()
                case "no" => 
              }
            }
          }
        }
        jsonRecurse()
      }//end of jsonReading()

      def readFilePrompt(){
        val z = database.name
        val a : String = scala.io.StdIn.readLine(s"You are in the $z Database.\n" +
          "Would you like to continue?(yes or no): ").toLowerCase()
        a match{
          case "yes" => jsonReading()
          case "no" => 
          case _ => {
            println("I'm sorry that isn't an option")
            readFilePrompt()
            println()
          }
        }
      }//End of readFilePrompt()
      readFilePrompt()
      directory()
    }//C:/Users/theod/AIORevature/Project0/JSONDogBreedDatasetv2.json



    //Select Collection
    def useCollection(){
      val b: String = scala.io.StdIn.readLine("Which Collection would you like to use?: ")
      try{
        collection = database.getCollection(s"$b")
        filterCollection = collection
      }
      catch{
        case ex: IllegalArgumentException => {
            println("Illegal Argument Exception")
            val a : String = scala.io.StdIn.readLine("Would you like to try again? (yes or no): ").toLowerCase()
            a match {
              case "yes" => useCollection()
              case "no" => directory
            }
          }
      }
      collection = database.getCollection(s"$b")
      filterCollection = collection
      directory()
    }
    
    //Show Collections
    def showCollections(){
      println()
      //This initiates the connection 
      database.listCollectionNames().results()
      space()
      //Prints the Collections
      database.listCollectionNames().printResults("Collections:\n")
      space()
      directory()
    }

    //Displays Collections without returning to the directory
    def showCollectionsInternal(){
      println()
      //This initiates the connection 
      database.listCollectionNames().results()
      space()
      //Prints the Collections
      database.listCollectionNames().printResults("Collections:\n")
      space()
    }

    def dropCollection(){
      val b: String = scala.io.StdIn.readLine("Which Collection would you like to Drop?: ")
      try{
        collection = database.getCollection(s"$b")
        collection.drop().headResult()
        //filterCollection = collection
      }
      catch{
        case ex: IllegalArgumentException => {
            println("Illegal Argument Exception")
            val a : String = scala.io.StdIn.readLine("Would you like to try again? (yes or no): ").toLowerCase()
            a match {
              case "yes" => dropCollection()
              case "no" => directory()
            }
          }
      }
      println()
      println(s"$b has succesfully been Dropped. Please select a collection to explore.")
      println()
      useCollection()
    }

     //An internal Document counter that doesn't return to directory
    def docCountInternal() = {
      collection.count().headResult()
    }

    //Returns the number of Documents in the Collection
    def docCount() = {
      space()        
      collection.count().printResults()
      space()
      directory
      }
      








  //Dataset specific functions//////////////////////////////////
    
    def addBreed() {

      val breedName: String =
      scala.io.StdIn.readLine("Please enter Breed Name: ")
      val group: String = scala.io.StdIn.readLine("Please enter Group: ") //pattern match this, try-catch
      print(s"Please enter $breedName's minimum height: ")
      val minHeight: Int = scala.io.StdIn.readInt()
      print(s"Please enter $breedName's maximum height: ")
      val maxHeight: Int = scala.io.StdIn.readInt()
      print(s"Please enter $breedName's minimum weight: ")
      val minWeight: Int = scala.io.StdIn.readInt()
      print(s"Please enter $breedName's maximum weight: ")
      val maxWeight: Int = scala.io.StdIn.readInt()
      print(s"Please enter $breedName's minimum life expectancy: ")
      val minLife: Int = scala.io.StdIn.readInt()
      print(s"Please enter $breedName's maximum life expectancy: ")
      val maxLife: Int = scala.io.StdIn.readInt()
      print(s"On a scale of 1-5, 1 being low, how affectionate is the $breedName?: ")
      val affection: Int = scala.io.StdIn.readInt()
      print(s"On a scale of 1-5, how well does the $breedName interact with young children?: ")
      val child: Int = scala.io.StdIn.readInt()
      print(s"On a scale of 1-5, how well does the $breedName interact with other dogs?: ")
      val otherDogs: Int = scala.io.StdIn.readInt()
      print(s"On a scale of 1-5, how much does the $breedName shed?: ")
      val sheddingLevel: Int = scala.io.StdIn.readInt()
      print(s"On a scale of 1-5, how much will a $breedName need to be groomed?: ")
      val grooming: Int = scala.io.StdIn.readInt()
      print(s"On a scale of 1-5, how much does the $breedName drool?: ")
      val drooling: Int = scala.io.StdIn.readInt()
      val coatType: String = scala.io.StdIn.readLine(s"What coat type does the $breedName breed have?: ") //pattern match this, try-catch
      val coatLength: String = scala.io.StdIn.readLine(s"What is the typical coat length for a $breedName?: ")
      print(s"On a scale of 1-5, 1 being low, how open to strangers is the $breedName?: ")
      val openness: Int = scala.io.StdIn.readInt()
      val playfulness: String = scala.io.StdIn.readLine(s"On a scale of 1-5, 1 being low, how playful is the $breedName?: ")
      print(s"On a scale of 1-5, how protective it the $breedName?: ")
      val protective: Int = scala.io.StdIn.readInt()
      print(s"On a scale of 1-5, how adaptable is the $breedName?: ")
      val adaptability: Int = scala.io.StdIn.readInt()
      print(s"On a scale of 1-5, how trainable is the $breedName?: ")
      val trainability: Int = scala.io.StdIn.readInt()
      print(s"On a scale of 1-5, how much energy does the $breedName have?: ")
      val energy: Int = scala.io.StdIn.readInt()
      print(s"On a scale of 1-5, how much does the $breedName bark?: ")
      val barking: Int = scala.io.StdIn.readInt()
      print(s"On a scale of 1-5, how much mental stimulation does the $breedName have?: ")
      val mentalStim: Int = scala.io.StdIn.readInt()
      print(s"Approximately how many $breedName's are you registering?: ")
      val akcRegistered: Int = scala.io.StdIn.readInt()
      val popRanking: Int = 196

      val firstcount = docCountInternal()
      println(s"Adding the $breedName to the Registry")
      val doc: Document = Document(
        "Popularity Ranking" -> popRanking,
        "Breed" -> breedName,
        "Group" -> group,
        "Min Height" -> minHeight,
        "Max Height" -> maxHeight,
        "Min Weight" -> minWeight,
        "Max Weight" -> maxHeight,
        "Min Life Expectancy" -> minLife,
        "Max Life Expectancy" -> maxLife,
        "Affection For Family" -> affection,
        "Good With Young Children" -> child,
        "Good With Other Dogs" -> otherDogs,
        "Shedding Level" -> sheddingLevel,
        "Coat Grooming Frequency" -> grooming,
        "Drooling Level" -> drooling,
        "Coat Type" -> coatType,
        "Coat Length" -> coatLength,
        "Openness To Strangers" -> openness,
        "Playfulness Level" -> playfulness,
        "Protective Nature" -> protective,
        "Adaptability" -> adaptability,
        "Trainability" -> trainability,
        "Energy" -> energy,
        "Barking" -> barking,
        "Mental Stimulation Needs" -> mentalStim,
        "Number of AKC Registered Puppies" -> akcRegistered
      )
      collection.insertOne(doc).printResults()
      val secondcount = docCountInternal()
      println(firstcount) //**
      println(secondcount) //**

      if (firstcount < secondcount) {
        println(s"Congratulations! you've just added the $breedName to the American Kennel club!")
      }
      space()
      directory()
    }//End of addBreed()


    def updateBreed() {
      var v: Any = ""
      var f: String = scala.io.StdIn.readLine("which Breed would you like to Update?: ")
      var a: String = scala.io.StdIn.readLine("Which Trait would you like to Modify?: ")
      var b: String = scala.io.StdIn.readLine("New Value: ")
      try {
       v = b.toInt
      }
      catch{
        case ex: NumberFormatException => v = b
      }
      finally{
        space()
        collection
          .find(equal("Breed",f))
          .projection(fields(include("Breed", a), excludeId()))
          .printResults()
        collection
          .updateOne(equal("Breed", f), set(a, v))
          .printHeadResult("Update Result: ")
        collection
          .find(equal("Breed",f))
          .projection(fields(include("Breed", a), excludeId()))
          .printResults()
        space()
        directory()       
      }
    }//End of updateBreed()


    def removeBreed() {

      //For the first iteration, jump to "//Establishing Breed"


      //Confirmation Recursion  
      def confirmationNo(){
        println("Would you like to change the breed or exit?")
        println("[1] Change Breed\n[2] Exit")
        val z : String = scala.io.StdIn.readLine()
        z match {
          case "1" => removeBreed
          case "2" =>
          case _ => {
            print("Im sorry, that's not an option")
            println()
            confirmationNo()
          }  
        }
      }//end of confirmationNo()

      //Establishing Breed
      var f: String = scala.io.StdIn.readLine("which Breed would you like to Remove?: ")

      //Confirmation of Deletion
      var a: String = scala.io.StdIn.readLine(s"Confirm: Remove the $f from the AKC? (yes or no): ")
      a match{
        case "yes"|"Yes"|"YES" => {
          val firstcount = docCountInternal()
          collection
            .deleteOne(equal("Breed", f))
            .printHeadResult("Delete Result: ")
          val secondcount = docCountInternal()
          space()

          //User friendly confirmation of deletion
          if (firstcount > secondcount) {
            println(
              s"The $f has been removed from the American Kennel Club Registry"
            )
          }
          if (firstcount == secondcount) {
            println(
              s"The $f has been NOT been removed. Confirm Spelling is Accurate"
            )
          }
        } 
        case "no"|"No"|"NO" => confirmationNo()
      }
    space()
    directory()
    }//End of removeBreed()


    def searchByBreedName() {

      val a = collection
        .find(equal("Breed", "Golden Retriever"))
        .projection(fields(include("Breed"), excludeId()))
        .printHeadResult()
      space()
    }//End of searchByBreedName()
    

    def searchByGroup() {

      //Establishing Group
      println("Groups: ")
      println()
      println(" -Toy\n -Sporting\n -Hound\n -Working\n -Terrier\n -Non-sporting\n -Herding\n")
      var group: String = scala.io.StdIn.readLine("Which group are you interested in? ").toLowerCase().trim()

      //Response Template
      def responseTemplate(group: String) {
        var a = group.capitalize
        println()
        println(s"Here are all the breeds in the $a Group!")
        filterCollection
          .find(equal("Group", a))
          .projection(fields(include("Popularity Ranking","Breed"), excludeId()))
          .printResults()

        //If the user would like to further refine the search
        var query = filterCollection.find(equal("Group", a)).results()
        refineFunction(query)    
      }

      //Group Matching
      group match {
        case "toy"|"sporting"|"hound"|"working" |"terrier"|"non-sporting"|"herding"  => responseTemplate(group)       
        case _ => {
          space()
          println("I'm sorry, that's not a group")
          println()
          println("Here are your choices:")
          print("Toy\nSporting\nHound\nWorking\nTerrier\nNon-sporting\nHerding\n")
          println()
          searchByGroup()
        }
      }     
    }// end of searchByGroup() 
























    def searchBySize() {
      var dimension = ""
      var unit = ""

      //Establishing Dimension
      def searchBySizePrompt(){
        var z: String = scala.io.StdIn.readLine("Height or Weight?: ").toLowerCase().trim()
        z match{
          case "weight" =>  dimension = "Weight"
          case "height" =>  dimension = "Height"
          case _ =>{
            println("I'm sorry, that's not an option")
            searchBySizePrompt()
          }
        }
      }//end of searchBySizePrompt()
      searchBySizePrompt()

      //Establishing Units
      if (dimension == "Weight") unit = "pounds"
      if (dimension == "Height") unit = "inches"

      //Establishing filter criteria
      def maxOrMin(){
        println()
        println(s"Are you more interested in exploring the Maximum or Minimum $dimension?:\n[1] Maximum\n[2] Minimum\n")
        println()
        val a: String = scala.io.StdIn.readLine("Please enter the corresponding number: ").trim()
        a match {
          case "1" => max()
          case "2" => min()
          case _ => {
            println("I'm sorry, that's not an option")
            maxOrMin()
          }
        }
      }//end of maxOrMin()
      maxOrMin()

      //The Max and Min functions need to be seperate becauase the collection filter (Results)
      // requires a different directive for each (lte for max and gte for min)
      //Max////////////////
      def max(){
        var value: Any = ""

        //In the event a Character or String is entered
        val a: String = scala.io.StdIn.readLine(s"Maximum $dimension($unit): ")
        try{
          value = a.toInt
        }
        catch{
          case ex: NumberFormatException =>{
           println("Please enter a Number")
           max()
          }
        }

        //Results
/*         
        filterCollection
          .find(lte(s"Max $dimension", value)).count().headResult
 */
        
        filterCollection
          .find(lte(s"Max $dimension", value))
          .projection(fields(include("Breed", "Popularity Ranking",s"Max $dimension"), excludeId()))
          .printResults()
        
        space()

        //If the user would like to further refine the search
        var query = filterCollection.find(lte(s"Max $dimension", value)).results()
        refineFunction(query)
   
      }// end of max()

      //Min////////////////
      def min(){
        //See max() for explination of code

        var value: Any = ""
        val a: String = scala.io.StdIn.readLine(s"Minimum $dimension($unit): ")
        try{
          value = a.toInt
        }
        catch{
          case ex: NumberFormatException =>{
           println("Please enter a Number")
           min()
          }
        }
        
        filterCollection
          .find(gte(s"Min $dimension", value))
          .projection(fields(include("Breed", "Popularity Ranking",s"Min $dimension"), excludeId()))
          .printResults()

        space()

        var query = filterCollection.find(gte(s"Min $dimension", value)).results()
        refineFunction(query)


      }//end of min()

    }//end of searchBySize()




    def searchByTrait(){
      println()
      var i = 0
      var dogTrait = ""

      //In order to prevent the search from being too restrictive, when the user selects "higher"
      //or "lower", the filter returns all breeds that score 3 and above or 3 and below, respectively.
      var value = 3

      //Lower response
      def lowerResponseTemplate(){
        println()
        //Results
        filterCollection
          .find(lte(dogTrait, value))
          .projection(fields(include("Breed", "Popularity Ranking"), excludeId()))
          .printResults()
        space()

        //Refine the search
        var query = filterCollection.find(lte(dogTrait, value)).results()
        refineFunction(query)

      }//End of lowResponseTemplate()

      //Higher response
      def higherResponseTemplate(){
        println()
        //Results
        filterCollection
          .find(gte(dogTrait, value))
          .projection(fields(include("Breed", "Popularity Ranking"), excludeId()))
          .printResults()
        space()

        //Refine the search
        var query = filterCollection.find(gte(dogTrait, value)).results()
        refineFunction(query)

      }//End of highResponseTemplate()


      //Start of searchByTrait()
      println("Please select a Trait ")
      println("[1] Affection for Family\n[2] Good with Young Children\n[3] Good with Other Dogs\n[4] Shedding Level\n" +
        "[5] Coat Grooming Frequency\n[6] Drooling Level\n[7] Openness to Strangers\n" +
        "[8] Playfulness Level\n[9] Protective Nature\n[10] Adaptability\n" +
        "[11] Trainability\n[12] Energy\n[13] Barking\n[14] Mental Stimulation Needs\n")
  
        def traitPrompt(){
          val a: String = scala.io.StdIn.readLine("Please enter the corresponding number: ").trim()
          a match{
            case "1" => dogTrait = "Affection For Family"
            case "2" => dogTrait = "Good With Young Children"          
            case "3" => dogTrait = "Good With Other Dogs"                  
            case "4" => dogTrait = "Shedding Level"       
            case "5" => dogTrait = "Coat Grooming Frequency"                 
            case "6" => dogTrait = "Drooling Level"              
            case "7" => dogTrait = "Openness To Strangers"       
            case "8" =>  dogTrait = "Playfulness Level"        
            case "9" => dogTrait = "Protective Nature"        
            case "10" =>  dogTrait = "Adaptability"        
            case "11" => dogTrait = "Trainability"       
            case "12" =>  dogTrait = "Energy"       
            case "13" =>  dogTrait = "Barking"        
            case "14" =>  dogTrait = "Mental Stimulation Needs"
            case _ => {
              println("I;m sorry, that's not an option")
              traitPrompt()
            }
          }
        }
        traitPrompt()
        println()
        def highLowprompt(){
          val b: String = scala.io.StdIn.readLine(s"Would you like $dogTrait to be Higher or Lower?(High or Low): ").toLowerCase().trim()
          b match{
            case "high"=> higherResponseTemplate() 
            case "low" =>  lowerResponseTemplate ()
            case _ =>{
              println("I'm sorry, that's not an option")
              highLowprompt()
            }
          }
        }
        highLowprompt()
    }//end of searchByTrait()

/* 
    def agg(){
      var aspect = ""
      var range = ""
      var eoA =""
      var maxChoice = false
      var minChoice = false

      def aggTree(){  

        def greet(){
          println("Are you interested in exploring Groups by:\n[1] Height\n[2] Weight\n[3] Life Expectancy?")
          print("Please enter the corresponding number: ")
          val a : String = scala.io.StdIn.readLine()
          a match{
            case "1" => aspect = "Height"
            case "2" => aspect = "Weight"
            case "3" => aspect = "Life Expectancy"
            case _ => {
              space()
              println("Oh no! It looks like that isn't an option!")
              greet()
            }        
          }
        }

        def maxOrMin(){
          println("Are you more interested in exploring the Maximums or Minimums of each Group?:\n[1] Maximum\n[2] Minimum\n")
          print("Please enter the corresponding number: ")
          val a: String = scala.io.StdIn.readLine()
          a match{
            case "1" =>{
             range = "Max"
             maxChoice = true
            }
            case "2" =>{
             range = "Min"
             minChoice = true
            }
            case _ => {
              space()
              println("Oh no! It looks like that isn't an option!")
              maxOrMin()
            }    
          }
        }

        def xTorAvg(){ ////Change Extremes to Upper and lower limits
          println("Are you more interested in exploring the Extremes or Averages of each Group?:\n[1] Extremes\n[2] Averages\n")
          print("Please enter the corresponding number: ")
          val a : String = scala.io.StdIn.readLine()
          a match{
            case "1" => extremeCase()
            case "2" => aggAvg()
            case _ => {
              space()
              println("Oh no! It looks like that isn't an option!")
              xTorAvg()
            }    
          }
        }


        def extremeCase(){
          if (maxChoice == true){          
            space()
            import org.mongodb.scala.model.Accumulators._
            println(s"Here are the $range $aspect's of all the Groups!\n")
            collection.
            aggregate(
              Seq(
                sort(orderBy(descending(s"$range $aspect"))),
                group(
                  "$Group",
                  max(s"$range $aspect", "$"+s"$range "+ s"$aspect"),            
                  first("Breed", "$Breed")
                    ),
                sort(orderBy(descending(s"$range $aspect"))),
              )
            )
            .printResults()                       
            }

          if (minChoice == true){           
            space()
            import org.mongodb.scala.model.Accumulators._
            println(s"Here are the $range $aspect's of all the Groups!\n")
            collection.
            aggregate(
              Seq(
                sort(orderBy(descending(s"$range $aspect"))),
                group(
                  "$Group",
                  min(s"$range $aspect", "$"+s"$range "+ s"$aspect"),            
                  last("Breed", "$Breed")
                    ),
                sort(orderBy(ascending(s"$range $aspect"))),
              )
            )
            .printResults()                       
            }           
        }



          def aggAvg(){
            import org.mongodb.scala.model.Accumulators._
            collection.
              aggregate(
                Seq(
                  sort(orderBy(descending(s"$range $aspect"))),
                  group(
                    "$Group",
                    avg(s"$range $aspect", "$"+s"$range "+ s"$aspect")           
                      ),
                  sort(orderBy(descending(s"$range $aspect"))),
                  project(fields(include(s"$range $aspect")))
                
                )
              )
              .printResults()
            //Alt project w/ 'round': project(fields(include(round("Max Weight",2))))
          }


        greet()
        maxOrMin()
        xTorAvg()
      }  

      //Call Function
      aggTree()
    }
 */




    //Program specifc functions////////////////////////////////
/* 
    If the user chooses to further refine their search after using a "searchBy" function, this directory
    is called rather than the global directory (directory())
*/
    def filterDirectory(){

      //!*!*!*!*!*!!*!*!*!*ADD AGG() TO THIS


      val a: String = scala.io.StdIn.readLine("Enter next Search Function: ")
      a match{
        case "exit" => directory()
        case "searchBySize()" => searchBySize()
        case "searchByGroup()" => searchByGroup()       
        case "searchByTrait()" => searchByTrait()
        case "--help" =>{
          println("searchByTrait()")
          println("searchBySize()")
          println("searchByGroup()")                   
          println()
          filterDirectory()
        }
        case _ =>{
         println("That's not a recognized function. Try --help")
         println()
         filterDirectory()
        }
      }
    }//End of filterDirectory()


    //Back-end Dirctory
    def directory(){
      
      //Resets the filterCollection to that of the Mongo collection
      filterCollection = collection
      println()
      val a: String = scala.io.StdIn.readLine("Enter Command: ")
      a match{
        case "exit" =>
        case "searchBySize()" => searchBySize()
        case "searchByGroup()" => searchByGroup()
        case "addBreed()" => addBreed()
        case "removeBreed()" => removeBreed() 
        case "updateBreed()" => updateBreed()
        case "searchByTrait()" => searchByTrait()
        case "dropDatabase()" => dropDatabase()
        case "showDatabases()" => showDatabases()
        case "dropCollection()" => dropCollection()
        case "showCollections()" => showCollections()
        case "useDatabase()" => useDatabase()
        case "readFile()" => readFile()
        case "docCount()" => docCount()
        case "useCollection()" => useCollection()
        case "groupBy()" => groupBy()
        case "--help" =>{
          println("searchByTrait()")
          println("searchBySize()")
          println("searchByGroup()")
          println("addBreed()")
          println("removeBreed()")
          println("updateBreed()")
          println("dropDatabase()")
          println("dropDatabase()")         
          println("showDatabases()")
          println("showCollections()")
          println("useDatabase()")
          println("readFile()")
          println("docCount()")
          println("useCollection()")
          //println("aggTree()")
          println("groupBy()")

          directory()
        }
        case _ =>{
         println("That's not a recognized function. Try --help")
         println()
         directory()
        }
      }

    }






    
    //agg()


/* 
    def aggAvg(){

      var range = "Max"
      var aspect = "Height"
      var aggregate = "$Energy"



      import org.mongodb.scala.model.Accumulators._
      println(s"Average $range $aspect by $aggregate")
      val results = collection.
            aggregate(
              Seq(
                sort(orderBy(descending(s"$range $aspect"))),
                group(
                  aggregate,
                  avg(s"$range $aspect", "$"+s"$range "+ s"$aspect")           
                    ),
                sort(orderBy(descending(s"$range $aspect"))),
                project(fields(include(s"$range $aspect")))
              
              )
            )
            .printResults()

     */   
/* 
    def aggAvg(){

      
      var aspect = "Barking"
      var aggregate = "$Energy"



      import org.mongodb.scala.model.Accumulators._
      println(s"Average $aspect by $aggregate")
      val results = collection.
            aggregate(
              Seq(
                sort(orderBy(descending(aspect))),
                group(
                  aggregate,
                  avg(aspect, "$"+s"$aspect")           
                    ),
                sort(orderBy(descending(aspect))),
                project(fields(include(aspect)))
              
              )
            )
            .printResults()

        */
        



        /* 
      collection
        .find()
        .sort(descending("Max Weight"))
        .projection(fields(include("Max Weight", "Energy"),excludeId()))
        .printResults()
    
 */

    def groupBy(){

    val playersDoc: Any =""
    var aggregate = ""
    var criteria = ""
    var z = 0




      
      

      def aspectList(){
        println("[1] Group\n[2] Min Height\n[3] Max Height\n" +
          "[4] Min Weight\n[5] Max Weight\n[6] Affection For Family\n" +
          "[7] Good With Young Children\n[8] Good With Other Dogs\n" +
          "[9] Shedding Level\n[10] Coat Grooming Frequency\n[11] Drooling Level\n" +
          "[12] Coat Type\n[13] Coat Length\n[14] Openness To Strangers\n" +
          "[15] Playfulness Level\n[16] Protective Nature\n[17] Adaptability\n" +
          "[18] Trainability\n[19] Energy\n[20] Barking\n[21] Mental Stimulation Needs\n"              
        )
      }

      def groupByPrompt(){   
        println   
        println("Select the Criteria you would like to Group By:")
        aspectList()
          def groupByPromptRecurse(){
          val a: String = scala.io.StdIn.readLine("Please enter the corresponding number: ")
          a match{
            case "1" => aggregate = "Group"
            case "2" => aggregate = "Min Height"          
            case "3" => aggregate = "Max Height"                  
            case "4" => aggregate= "Min Weight"       
            case "5" => aggregate = "Max Weight"                 
            case "6" => aggregate= "Affection For Family"
            case "7" => aggregate= "Good With Young Children"          
            case "8" => aggregate= "Good With Other Dogs"                  
            case "9" => aggregate= "Shedding Level"       
            case "10" => aggregate = "Coat Grooming Frequency"                 
            case "11" => aggregate= "Drooling Level" 
            case "12" =>  aggregate = "Coat Type"        
            case "13" =>  aggregate= "Coat Length"
            case "14" => aggregate = "Openness To Strangers"       
            case "15" =>  aggregate = "Playfulness Level"        
            case "16" =>  aggregate = "Protective Nature"        
            case "17" =>   aggregate= "Adaptability"        
            case "18" =>  aggregate= "Trainability"       
            case "19" =>   aggregate= "Energy"       
            case "20" =>   aggregate= "Barking"        
            case "21" =>   aggregate= "Mental Stimulation Needs"
            case _ => {
              println("I'm sorry, that's not an option")
              groupByPromptRecurse()
            }
          }
          
        }
        groupByPromptRecurse()
        avgCriteriaGroup()
      }
          

    def avgCriteriaGroup(){

      println(s"Fill in the Blank: You would like to see the average ______ by $aggregate")

      aspectList()
        def avgCriteriaGroupRecurse(){
          val a: String = scala.io.StdIn.readLine("Please enter the corresponding number: ")
          a match{
            case "1" =>{
              criteria = "Group" 
              z=1
            }
            case "2" => {
              criteria = "Min Height"          
              z=2
            }
            case "3" =>{
            criteria = "Max Height"
            z=3
            }                   
            case "4" =>{
             criteria = "Min Weight" 
             z=4
            }      
            case "5" =>{
            criteria = "Max Weight"
            z=5
            }             
            case "6" =>{
             criteria = "Affection For Family"
             z=6
            }
            case "7" =>{
            criteria = "Good With Young Children"  
            z=7  
            }       
            case "8" =>{
            criteria = "Good With Other Dogs"  
            z=8
            }                 
            case "9" =>{
            criteria = "Shedding Level"   
            z=9
            }     
            case "10" =>{
            criteria = "Coat Grooming Frequency"  
            z=10
            }                
            case "11" =>{
            criteria = "Drooling Level" 
            z=11
            } 
            case "12" =>{
            criteria = "Coat Type"
            z=12
            }          
            case "13" =>{
             criteria = "Coat Length"
             z=13
            } 
            case "14" =>{
            criteria = "Openness To Strangers" 
            z=14
            }       
            case "15" =>{
             criteria = "Playfulness Level"   
             z=15
            }      
            case "16" =>{
            criteria = "Protective Nature"   
            z=16
            }      
            case "17" =>{
             criteria = "Adaptability"     
             z=17
            }    
            case "18" =>{
            criteria = "Trainability"    
            z=18
            }    
            case "19" =>{
            criteria = "Energy"
            z=19
            }         
            case "20" =>{
            criteria = "Barking"  
            z=20
            }        
            case "21" =>{
            criteria = "Mental Stimulation Needs"
            z=21
            }  
            case _ => {
              println("I;m sorry, that's not an option")
              avgCriteriaGroupRecurse()
            }
          }
        }
        avgCriteriaGroupRecurse()
        traitAgg()
 
    }//End of avgCriteriaGroup()




    def traitAgg(){
    
    import org.mongodb.scala.model.Accumulators._
    println(s"Average $aggregate by $criteria")
    val results = collection.
          aggregate(
            Seq(              
              group(
                "$"+ aggregate,
                avg(criteria,"$"+ criteria),                            
                  ),
              sort(descending(criteria)),           
            )
          )
          .results()

          import net.liftweb.json._
          implicit val formats = DefaultFormats
          for (dog <- results){
            val jsonString = dog.toJson().toLowerCase().replaceAll("\\s","")
            val jValue = parse(jsonString)

            z match{
            case 1 =>  {
              //criteria = "Group"
              case class Dog(_id: String, group: String)
              val playersDoc = jValue.extract[Dog]
              var b = (f"$aggregate: ${playersDoc._id}, $criteria: ${playersDoc.group}").split(" ").map(_.capitalize).mkString(" ")
              println(b)
            }
            case 2 => {
              //criteria = "Min Height"
              case class Dog(_id: String, minheight: Double)
              val playersDoc = jValue.extract[Dog]
              var b = (f"$aggregate: ${playersDoc._id}, $criteria: ${playersDoc.minheight}%.2f").split(" ").map(_.capitalize).mkString(" ")
              println(b)            
            }         
            case 3 => {
              //criteria = "Max Height"
              case class Dog(_id: String, maxheight: Double)          
              val playersDoc = jValue.extract[Dog]
              var b = (f"$aggregate: ${playersDoc._id}, $criteria: ${playersDoc.maxheight}%.2f").split(" ").map(_.capitalize).mkString(" ")
              println(b)
           }                 
            case 4 =>{
              //criteria = "Min Weight"
              case class Dog(_id: String, minweight: Double)          
              val playersDoc = jValue.extract[Dog]
              var b = (f"$aggregate: ${playersDoc._id}, $criteria: ${playersDoc.minweight}%.2f").split(" ").map(_.capitalize).mkString(" ")
              println(b) 
             }      
            case 5 =>{
              //criteria = "Max Weight"
              case class Dog(_id: String, maxweight: Double)          
              val playersDoc = jValue.extract[Dog]
              var b = (f"$aggregate: ${playersDoc._id}, $criteria: ${playersDoc.maxweight}%.2f").split(" ").map(_.capitalize).mkString(" ")
              println(b)
            }              
            case 6 =>{             
              //criteria = "Affection For Family"
              case class Dog(_id: String, affectionforfamily: Double)          
              val playersDoc = jValue.extract[Dog]
              var b = (f"$aggregate: ${playersDoc._id}, $criteria: ${playersDoc.affectionforfamily}%.2f").split(" ").map(_.capitalize).mkString(" ")
              println(b)
            } 
            case 7 =>{
              //criteria = "Good With Young Children"
              case class Dog(_id: String, goodwithyoungchildren: Double)          
              val playersDoc = jValue.extract[Dog]
              var b = (f"$aggregate: ${playersDoc._id}, $criteria: ${playersDoc.goodwithyoungchildren}%.2f").split(" ").map(_.capitalize).mkString(" ")
              println(b)
              }           
            case 8 =>{
              //criteria = "Good With Other Dogs"
              case class Dog(_id: String, goodwithotherdogs: Double)          
              val playersDoc = jValue.extract[Dog]
              var b = (f"$aggregate: ${playersDoc._id}, $criteria: ${playersDoc.goodwithotherdogs}%.2f").split(" ").map(_.capitalize).mkString(" ")
              println(b)
            }                   
            case 9 =>{
              //criteria = "Shedding Level"
              case class Dog(_id: String, sheddinglevel: Double)          
              val playersDoc = jValue.extract[Dog]
              var b = (f"$aggregate: ${playersDoc._id}, $criteria: ${playersDoc.sheddinglevel}%.2f").split(" ").map(_.capitalize).mkString(" ")
              println(b)
            }        
            case 10 =>{
              //criteria = "Coat Grooming Frequency"
              case class Dog(_id: String, coatgroomingfrequency: Double)          
              val playersDoc = jValue.extract[Dog]
              var b = (f"$aggregate: ${playersDoc._id}, $criteria: ${playersDoc.coatgroomingfrequency}%.2f").split(" ").map(_.capitalize).mkString(" ")
              println(b)
            }                  
            case 11 =>{
              //criteria = "Drooling Level"
              case class Dog(_id: String, droolinglevel: Double)          
              val playersDoc = jValue.extract[Dog]
              var b = (f"$aggregate: ${playersDoc._id}, $criteria: ${playersDoc.droolinglevel}%.2f").split(" ").map(_.capitalize).mkString(" ")
              println(b)
            } 
            case 12 =>{
              //criteria = "Coat Type"
              case class Dog(_id: String, coattype: String)          
              val playersDoc = jValue.extract[Dog]
              var b = (f"$aggregate: ${playersDoc._id}, $criteria: ${playersDoc.coattype}").split(" ").map(_.capitalize).mkString(" ")
              println(b)
            }          
            case 13 =>{
              //criteria = "Coat Length"
              case class Dog(_id: String, coatlength: String)          
              val playersDoc = jValue.extract[Dog]
              var b = (f"$aggregate: ${playersDoc._id}, $criteria: ${playersDoc.coatlength}").split(" ").map(_.capitalize).mkString(" ")
              println(b)
            }  
            case 14 =>{ 
              //criteria = "Openness To Strangers"
              case class Dog(_id: String, opennesstostrangers: Double)          
              val playersDoc = jValue.extract[Dog]
              var b = (f"$aggregate: ${playersDoc._id}, $criteria: ${playersDoc.opennesstostrangers}%.2f").split(" ").map(_.capitalize).mkString(" ")
              println(b)
            }       
            case 15 =>{
              //criteria = "Playfulness Level"
              case class Dog(_id: String, playfulnesslevel: Double)          
              val playersDoc = jValue.extract[Dog]
              var b = (f"$aggregate: ${playersDoc._id}, $criteria: ${playersDoc.playfulnesslevel}%.2f").split(" ").map(_.capitalize).mkString(" ")
              println(b)
            }          
            case 16 =>{
              //criteria = "Protective Nature"
              case class Dog(_id: String, protectivenature: Double)          
              val playersDoc = jValue.extract[Dog]
              var b = (f"$aggregate: ${playersDoc._id}, $criteria: ${playersDoc.protectivenature}%.2f").split(" ").map(_.capitalize).mkString(" ")
              println(b)
            }        
            case 17 =>{
            //criteria = "Adaptability"
              case class Dog(_id: String, adaptability: Double)          
              val playersDoc = jValue.extract[Dog]
              var b = (f"$aggregate: ${playersDoc._id}, $criteria: ${playersDoc.adaptability}%.2f").split(" ").map(_.capitalize).mkString(" ")
              println(b)
            }         
            case 18 =>{
              //criteria = "Trainability"
              case class Dog(_id: String, trainability: Double)          
              val playersDoc = jValue.extract[Dog]
              var b = (f"$aggregate: ${playersDoc._id}, $criteria: ${playersDoc.trainability}%.2f").split(" ").map(_.capitalize).mkString(" ")
              println(b)
            }        
            case 19 =>{
              //criteria = "Energy"
              case class Dog(_id: String, energy: Double)          
              val playersDoc = jValue.extract[Dog]
              var b = (f"$aggregate: ${playersDoc._id}, $criteria: ${playersDoc.energy}%.2f").split(" ").map(_.capitalize).mkString(" ")
              println(b)
            }         
            case 20 =>{
              //criteria = "Barking"
              case class Dog(_id: String, barking: Double)          
              val playersDoc = jValue.extract[Dog]
              var b = (f"$aggregate: ${playersDoc._id}, $criteria: ${playersDoc.barking}%.2f").split(" ").map(_.capitalize).mkString(" ")
              println(b)
            }          
            case 21 =>{
              //criteria = "Mental Stimulation Needs"
              case class Dog(_id: String, mentalstimulationneeds: Double)          
              val playersDoc = jValue.extract[Dog]
              var b = (f"$aggregate: ${playersDoc._id}, $criteria: ${playersDoc.mentalstimulationneeds}%.2f").split(" ").map(_.capitalize).mkString(" ")
              println(b)
            }  
            /* case _ => {
              println("I;m sorry, that's not an option")
              
            } */
          }


            
/* 
            if (z == 5){
            case class DogTEST(_id: String, maxweight: Double)
            val playersDoc = jValue.extract[DogTEST]
            var b = (f"$aggregate: ${playersDoc._id}, $criteria: ${playersDoc.maxweight}%.2f").split(" ").map(_.capitalize).mkString(" ")
            println(b)
 */




            






            }
            



            
            
            

          




        }

        
        groupByPrompt()




        directory()
    }//End of groupBy()
     //groupBy()







/* 
import org.mongodb.scala.model.Accumulators._
    println(s"Average $aggregate by $criteria")
    val results = collection.
          aggregate(
            Seq(
              //sort(descending("Energy")),
              group(
                aggregate,
                avg(criteria,"$"+ criteria),                            
                  ),
              sort(descending(criteria)),
              //sort(orderBy(descending("Max Weight"))),
            
              //project(fields(include("Max Weight", "Energy"),excludeId()))
            
            )
          )
          .printResults() */












/* 

    def jsonParse(){

      var criteria = "Max Weight"
      var aggregate = "Group"



      import org.mongodb.scala.model.Accumulators._

      import org.mongodb.scala.model.Accumulators._
      println(s"Average $aggregate by $criteria")
    val results = collection.
          aggregate(
            Seq(              
              group(
                "$"+ aggregate,
                avg(criteria,"$"+ criteria),                            
                  ),
              sort(descending(criteria)),           
            )
          )
          .results()



         //FORMATING/////////////////////////// 
        import net.liftweb.json._
        implicit val formats = DefaultFormats
        for (dog <- results){
          val jsonString = dog.toJson().toLowerCase().replaceAll("\\s","")
          val jValue = parse(jsonString)

          case class Dog(_id: String, maxweight: Double)          
          val playersDoc = jValue.extract[Dog]
          var b = (f"$aggregate: ${playersDoc._id}, $criteria: ${playersDoc.maxweight}%.2f").split(" ").map(_.capitalize).mkString(" ")
          println(b)
        }

        case class Dog1(_id: String, group: Double)
        case class Dog2(_id: String, minheight: Double)
        case class Dog3(_id: String, maxheight: Double)
        case class Dog4(_id: String, minweight: Double)
        case class Dog5(_id: String, maxweight: Double)
        case class Dog6(_id: String, affectionforfamily: Double)
        case class Dog7(_id: String, goodwithyoungchildren: Double)
        case class Dog8(_id: String, goodwithotherdogs: Double)
        case class Dog9(_id: String, sheddinglevel: Double)
        case class Dog10(_id: String, coatgroomingfrequency: Double)
        case class Dog11(_id: String, droolinglevel: Double)
        case class Dog12(_id: String, coattype: Double)
        case class Dog13(_id: String, coatlength: Double)
        case class Dog14(_id: String, opennesstostrangers: Double)
        case class Dog15(_id: String, playfulnesslevel: Double)
        case class Dog16(_id: String, protectivenature: Double)
        case class Dog17(_id: String, adaptability: Double)
        case class Dog18(_id: String, trainability: Double)
        case class Dog19(_id: String, energy: Double)
        case class Dog20(_id: String, barking: Double)
        case class Dog21(_id: String, mentalstimulationneeds: Double)

















/* 

        import net.liftweb.json._
        //implicit val formats = DefaultFormats

        // Class mapped to the JSON output of the Players collection.
        case class Dog(_id: String, Energy: Double)

        println("Dog as JSON...")

        for (dog <- results){
        // Convert the doc into a proper JSON string.
        val jsonString = dog.toJson()
        println(jsonString)



        // Convert the JSON string into a JSON object.
        val jValue = parse(jsonString)
          // create a Player object from the string
        val playersDoc = jValue.extract[Dog]

        // Calculate the total points and print.
      //val total = playersDoc.breed + playersDoc.energy
      println(s"Player: ${playersDoc._id}, goals: ${playersDoc.energy}, assists: ${playersDoc.maxHeight}")
     

        }
        

 */

      } */
     //jsonParse()






      //Alt project w/ 'round': project(fields(include(round("Max Weight",2))))














    //First function of the program
    showDatabases()
    
    //Closes connection to Mongo Client
    client.close()
  }
}

