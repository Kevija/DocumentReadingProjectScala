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