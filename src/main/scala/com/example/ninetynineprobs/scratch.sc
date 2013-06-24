/**
 * Created with IntelliJ IDEA.
 * User: jarodbelshaw
 * Date: 1/28/13
 * Time: 10:26 AM
 * To change this template use File | Settings | File Templates.
 */

val p12: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))

for (n <- p12) yield {  for(s <- 1 to n._1) yield (s)  }

val p13: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)


p13.map(e => ((List(e)++p13.tail.takeWhile(_ == e)).length, e))


