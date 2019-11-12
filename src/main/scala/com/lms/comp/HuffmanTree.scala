package com.lms.comp

import com.github.benmanes.caffeine.cache.Caffeine
import scalacache._
import com.lms.comp.util.HexByte

import scala.annotation.tailrec
import scala.collection.immutable.Queue

class TData(data: Array[Byte]) {
  lazy val hash = HexByte.toHex(data)
  def getBytes(): Array[Byte] = data
  override def hashCode(): Int = hash.hashCode
  override def toString: String = hash
}

case class EmptyNode() extends HuffmanTree
case class TreeNode(node: Option[TData], weight: Option[Int],
                    left: HuffmanTree, right: HuffmanTree) extends HuffmanTree

  class HuffmanTree {

    def flatten(): Seq[TreeNode] = {
      this match {
        case TreeNode(d, w, l, r) => (this +: l.flatten ++: r.flatten).asInstanceOf[Seq[TreeNode]]
        case EmptyNode() => List[TreeNode]()
      }
    }

    def build(xs: List[TData]): HuffmanTree = {

      @tailrec
      def huffmanAlgorithm(xs: Seq[TreeNode]): TreeNode = {
          xs.sortWith(_.weight.get < _.weight.get) match {
          case head::Nil => head
          case a::b::tail =>
              val weight = Some(a.weight.get + b.weight.get)
              val parent = TreeNode(None, weight, a, b)
              huffmanAlgorithm(parent +: tail)
        }
      }

      huffmanAlgorithm {
        xs.distinctBy(_.hashCode())
          .map(e => (e, xs.count(_.hashCode() equals e.hashCode())))
          .map { case (node: TData, weight: Int) =>
            TreeNode(Some(node), Some(weight), EmptyNode(), EmptyNode())
          }
      }
    }

    val cache = Caffeine.newBuilder.build[String, Entry[Seq[Boolean]]]

    @throws(classOf[NoSuchElementException])
    def encode(data: TData, acc: Seq[Boolean] = Seq[Boolean]()): Seq[Boolean] = {

      cache.getIfPresent(data.toString) match {
        case null => {
          val bits = this match {
            case TreeNode(n, w, l, r) =>
              if (n.map(_.hashCode()).contains(data.hashCode()))
                acc
              else {
                try r.encode(data, acc :+ true) catch { case e: NoSuchElementException =>
                  try l.encode(data, acc :+ false) catch { case e: NoSuchElementException =>
                    throw e
                  }
                }
              }
            case EmptyNode() =>
              throw new NoSuchElementException(data.toString)
          }
          cache.put(data.toString, Entry(bits, expiresAt = None))
          bits
        }
        case v => v.value
      }

    }

    def encodeSeq(data: Array[Byte]): Seq[Boolean] =
      data.grouped(1).flatMap(x=> encode(new TData(x))).toSeq

    @tailrec
    final def decode(data: Seq[Boolean]): Option[TData] = {
      this match {
        case TreeNode(n, w, l, r) => {
          data match {
            case Nil => n
            case h :: tail =>
              if (h) r.decode(tail) else l.decode(tail)
          }
        }
        case EmptyNode() => None
      }
    }

    @tailrec
    final def decodeSeq(binary: List[Boolean], acc: Queue[TData] = Queue(), root: HuffmanTree = this) : Seq[TData] = {
      this match {
        case TreeNode(n, w, l, r) if n.nonEmpty =>
          binary match {
            case x::xs =>  root.decodeSeq(binary, acc:+n.get, root)
            case Nil => acc:+n.get
          }
        case TreeNode(n, w, l, r) =>
          binary match {
            case x::xs if x => r.decodeSeq(xs, acc, root)
            case x::xs => l.decodeSeq(xs, acc, root)
            case Nil => acc
          }
        case EmptyNode() =>
          Seq[TData]()
      }
    }

    override def toString: String = {
      this match {
        case TreeNode(n, w, l, r) =>
          (l, r) match {
            case (EmptyNode(), EmptyNode()) => s"${n.getOrElse("")}"
            case _ => s"${n.getOrElse("")}($l,$r)"
        }
        case EmptyNode() => ""
      }
    }

    def fromString(s: String): HuffmanTree = {
      import scala.util.parsing.combinator.RegexParsers
      object TreeParser extends RegexParsers {
        override def skipWhitespace = false
        //def node: Parser[A] = """(\\\\|[^(),\\]|\\,|\\\(|\\\))""".r ^^ {
        //  res => (if (res.startsWith("\\")) res.drop(1) else res).charAt(0).asInstanceOf[A]
        //}
        def node: Parser[TData] = """([^(),]{1,})""".r ^^ {
          res => new TData(HexByte.toBytes(res))
        }
        def subtrees: Parser[(HuffmanTree, HuffmanTree)] = "(" ~ tree.? ~ "," ~ tree.? ~ ")" ^^ {
          case (start ~ left ~ comma ~ right ~ stop) => (left.getOrElse(EmptyNode()), right.getOrElse(EmptyNode()))
        }
        def tree: Parser[HuffmanTree] = (node ~ subtrees | (subtrees | node)) ^^ {
          case (n: TData) ~ ((l: HuffmanTree, r: HuffmanTree)) =>
             TreeNode(Some(n), None, l, r)
          case (l: HuffmanTree, r: HuffmanTree) =>
             TreeNode(None, None, l, r)
          case n: TData =>
             TreeNode(Some(n), None, EmptyNode(), EmptyNode())
        }
        def apply(input: String) = parseAll(tree, input) match {
          case Success(result, _) => result
          case failure: NoSuccess => scala.sys.error(failure.msg)
        }
      }
      TreeParser.apply(s)
    }

  }