package stackoverflow

import org.scalatest.{BeforeAndAfterAll, FunSuite}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import org.apache.spark.rdd.RDD
import java.io.File

import org.apache.spark.storage.StorageLevel

@RunWith(classOf[JUnitRunner])
class StackOverflowSuite extends FunSuite with BeforeAndAfterAll {


  lazy val testObject = new StackOverflow {
    override val langs =
      List(
        "JavaScript", "Java", "PHP", "Python", "C#", "C++", "Ruby", "CSS",
        "Objective-C", "Perl", "Scala", "Haskell", "MATLAB", "Clojure", "Groovy")
    override def langSpread = 50000
    override def kmeansKernels = 45
    override def kmeansEta: Double = 20.0D
    override def kmeansMaxIterations = 120
  }

  test("testObject can be instantiated") {
    val instantiatable = try {
      testObject
      true
    } catch {
      case _: Throwable => false
    }
    assert(instantiatable, "Can't instantiate a StackOverflow object")
  }

  private val lines = "1,27233496,,,0,C#\n1,23698767,,,9,C#\n1,5484340,,,0,C#\n2,5494879,27233496,5484340,1,".split('\n')

  test("grouped postings") {
    import StackOverflow._

    val postings = sc.makeRDD( lines )
    val raw = testObject.rawPostings(postings)
    val grouped = testObject.groupedPostings(raw)
    assert(grouped.collect.length > 0)
  }

  test("scored postings") {
    import StackOverflow._

    val lines   = sc.textFile("src/main/resources/stackoverflow/stackoverflow.csv").persist(StorageLevel.MEMORY_AND_DISK)
    val raw     = testObject.rawPostings(lines)
    val grouped = testObject.groupedPostings(raw)
    val scored  = testObject.scoredPostings(grouped).collect()
    assert(scored.length == 2121822)
  }

  def question(tag: String, idSeq: Int, accepted: Option[Answer]): Question = {
    Posting(1, idSeq + 1, accepted.map(_.id), None, (math.random * 100).toInt, Some(tag))
  }

  def answer(questionId: QID, idSeq: Int): Answer = {
    Posting(2, idSeq + 1, None, Some(questionId), (math.random * 100).toInt, None)
  }
}
