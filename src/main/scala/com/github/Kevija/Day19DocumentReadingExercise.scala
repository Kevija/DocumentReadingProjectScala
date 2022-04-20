package com.github.Kevija

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

case class Document(title: String = "", author: String = "", url: String = "", rows: Array[String] = Array[String]()) {
  val rowCount:Int = rows.length
  val wordsInRows:Array[Int] = rows.map(_.length)
  val wordCount:Int = wordsInRows.sum
  val timestamp: String = LocalDateTime.now.format(DateTimeFormatter.ofPattern("YYYY_MM_dd_HH_mm"))

  def getFileName: String = {
    author.slice(0,10) + "_" + title.slice(0,15) + "_" + timestamp  + ".txt"
  }

  def appendLines: Array[String] = {
    val firstRow:String = s"URL: $url"
    val secondRow:String = s"Author: $author"
    val thirdRow:String = s"Title: $title"
    val newRows: Array[String] = Array(firstRow, secondRow, thirdRow, "\n\n\n") ++ rows
    newRows
  }

  def save(dst: String = "", folder: String = "src/resources/texts"): Unit = {
    val newLines = appendLines
    val dstPath: String = if (dst == "") folder + "//" + getFileName else folder + "//" + dst
    Util.saveLines(dstPath, newLines)
  }
}

object Day19DocumentReadingExercise extends App {

  val filePath = "src/resources/webpages.txt"
  val fileLines = Util.getLinesFromFile(filePath)
  val urlList = for (line <- fileLines) yield {
    if (line.startsWith("http://") || line.startsWith("https://")) line
    else "https://" + line

  }
  def getDocumentsFromUrls(urls:Array[String]):Array[Document] = {
    val documentArray = for (url <- urls) yield {
      val rows = Util.getLinesFromWeb(url)
      val title = if (url.endsWith(".txt")) GutenbergUtil.getTitle(rows) else {url.split("\\p{Punct}").last}
      val author = if (url.endsWith(".txt")) GutenbergUtil.getAuthor(rows) else {url.split("/")(2)}
      Document(title, author, url, rows)
    }
    documentArray
  }

  val myDocuments = getDocumentsFromUrls(urlList)
  for (doc <- myDocuments) {
    val dstPath = doc.getFileName
    doc.save(dstPath)
    println(s"${doc.title} - ${doc.author}")
    Thread.sleep(200)
  }

}



//TODO create Document class (or case class whichever you prefer)
//class should contain following constant fields to be passed as parameters upon creation:
// title:String = ""
// author:String = ""
// url:String = ""
// rows:Array[String] = Array[String]()

// TODO upon creation in constructor (main body of class) - the following constant fields should be created
// rowCount:Int - rows.length
// wordCount:Int - contains a count of all the words in rows

//TODO add a single method save(dst:String = "", folder:String="src/resources/texts"):Unit
//will write to the folder/dst file - remember to add the extra slash
//will write s"URL: $url" as first row
// similarly will write Author: actual author as 2nd row
//will write Title: title as 3rd row
//will write 3 newlines
//will will write all rows

//if dst is empty string - create a file name
//if author AND title are non empty -
//create file name such as Doyle_Adventures.txt (10 letters max from author, 15 letters max from title
// bonus - add timestamp in file name
// something like this Doyle_Adventures_2022_4_16_15_06.txt
// https://stackoverflow.com/questions/48378006/how-to-get-current-timestamp-in-scala-as-a-string-without-spaces

//object Day19DocumentReadingExercise extends App {
//TODO create a program that reads web addresses from a file and downloads multiple files with some changes
//check for system arguments (see Day14commandArguments )
//use first argument as filePath to process
//otherwise default filePath will be src/resources/webPages.txt

//TODO read all lines from filePath
//assume each line is one URL
//bonus: add https:// prefix if one is not present in each line - if http:// OR https:// is present DO NOTHING

//TODO read all URLs - save into array of Document objects
//TODO call save method on all members of the array
//you can let save figure out the names automatically (if you made this functionality)
//or you can pass some dst name here as well
//Use slight delay between each URL read such as  Thread.sleep(200) //200ms delay

//TODO test it by creating your own 10 URL text file (can use Project Gutenberg can use some other sites and saving it as src/resources/webPages.txt

