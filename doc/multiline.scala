// four statements, each on two lines
if (x > 0)
  x = x - 1

while (x > 0)
  x = x / 2

for (x <- 1 to 10)
  println(x)

type
  IntList = List[Int]


// anonymous class:
new Iterator[Int]
{
  private var x = 0
  def hasNext = true
  def next = { x += 1; x }
}

// object creation + local block:
new Iterator[Int]

{
  private var x = 0
  def hasNext = true
  def next = { x += 1; x }
}


// single expression:
x < 0 ||
x > 10

// two expressions:
x < 0 ||

x > 10


// single, curried function definition:
def func(x: Int)
        (y: Int) = x + y

// abstract function definition + syntactically illegal statement:
def func(x: Int)

        (y: Int) = x + y


// attributed definition
@serializable
protected class Data { }

// attribute + separate statement
@serializable

protected class Data { }
