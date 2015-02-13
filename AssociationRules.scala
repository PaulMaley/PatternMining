// ItemSet
// Item

//import scala.collection.mutable.Set
import scala.util.Sorting

type Item = Char //Int
type ItemSet = Set[Item]
type Database = List[ItemSet]

// Transaction database
// Implemented as a list as duplicates are possible
//val TDB = List(Set(1,2,4), Set(1,3), Set(2,3,4), Set(1,3), Set(1,3,8))
//val TDB = List(Set('A','B','D'), Set('A','C'), Set('B','C','D'), Set('A','C'), Set('A','C','H'))
//val TDB = List(Set('A','C','D'), Set('B','C','E'), Set('A','B','C','E'), Set('B','E'))

val TDB = List(Set('F','A','C','D','G','I','M','P'),
			   Set('A','B','C','F','L','M','O'),
			   Set('B','F','H','J','O','W'),
			   Set('B','C','K','S','P'),
			   Set('A','F','C','E','L','P','M','N'))


// Want the dbase and values such as minsup to be implicit

def support(database: Database) = (s: ItemSet) => 
	(database.filter(e => s subsetOf e).size.toFloat / database.size)
	
def singletons(db: Database) = db.reduceLeft( _ union _ ).map( e => Set(e))

def frequent(database: Database, minsup: Float) = (s:ItemSet) => 
	support(database)(s) > minsup 
	
val sup = support(TDB)		// Tzpe is ItemSet => Float
val f = frequent(TDB, 0.49f)	// Type is ItemSet => Boolean

// Apriori algorithm

// 1-Set candidates
val f1 = singletons(TDB).filter(f)
val f1Set = f1.reduceLeft(_ union _) 


def generate(cs: Set[ItemSet]) = 
	(for {c <- cs
		 x = f1.filter( e => ! (e subsetOf c) ).map(_ union c)
	} yield x).flatten

def aPriori(accepted: Set[ItemSet], candidates: Set[ItemSet]): Set[ItemSet] = {
	val z = candidates.filter(f)	// Filter out non-frequent candidates 
	if (z.isEmpty) accepted 		// No more candidates ... finished
	else aPriori(accepted.union(z), generate(z))	
}
	
	
/*
def generate(cs: Set[ItemSet]) = 
	for (c <- cs){
		println(c)
		val x = f1.filter( e => ! (e subsetOf c) )
		val z = x.map(_ union c)
		println(x)
		println(z)
	}
*/


val c2 = Set(Set(3,4), Set(7,8))

// FP Tree implementation




// Sort singletons according to support
val s1 = singletons(TDB).map(s => (s,sup(s)))

// Need to define an ordering on Item 
object ItemOrdering extends Ordering[Item] {
	def compare(a: Item, b:Item) = sup(Set(a)) compare sup(Set(b))
}

object SupOrdering extends Ordering[(ItemSet, Float)] {
	def compare(a:(ItemSet, Float), b:(ItemSet, Float)) = a._2 compare b._2
}

// Empty ordered set ... check Syntax
val OE = SortedSet(ItemOrdering).Empty
// Then do OE ++ unorderedSet to order elements

val sf1 = s1.toArray
Sorting.quickSort(sf1)(SupOrdering) // Mutating ordering

// Filter the database into a list of ordered frequent 
for{ tid <- TDB
	 xa = tid.filter( e => f1Set contains e ).toArray
	 //println(xa(0))
	 } yield Sorting.quickSort(xa)(ItemOrdering)
