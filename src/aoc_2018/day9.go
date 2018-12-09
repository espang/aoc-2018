package main

import (
	"fmt"
  "time"
)

type Element struct {
	Last  *Element
	Next  *Element
	Value int
}

func (e *Element) Add(v int) (*Element, int) {
	if v%23 == 0 {
		return e.addMod23(v)
	}
	return e.addNormal(v), 0
}

func (e *Element) addMod23(v int) (*Element, int) {
	for i := 0; i < 7; i++ {
		e = e.Last
	}
	val := e.Value
	// remove e
	e.Last.Next = e.Next
	e.Next.Last = e.Last
	return e.Next, val + v
}

func (e *Element) addNormal(v int) *Element {
	n1 := e.Next
	n2 := n1.Next

	added := &Element{
		Value: v,
		Last:  n1,
		Next:  n2,
	}
	n1.Next = added
	n2.Last = added
	return added
}

func PlayGame(players, valueOfLastMarble int) Points {
	current := &Element{
		Value: 0,
	}
	current.Next = current
	current.Last = current

	points := map[int]int{}
	var p int
	for i := 1; i <= valueOfLastMarble; i++ {
		current, p = current.Add(i)
		points[i%players] += p
	}
	return points
}

type Points map[int]int

func (p Points)max() int {
	max := 0
	for _, v := range p {
		if v > max {
			max = v
		}
	}
	return max
}

func main() {
	g1 := PlayGame(9, 23)
	fmt.Println(g1.max())

	g2 := PlayGame(13, 7999)
	fmt.Println(g2.max())

	start := time.Now()
	g3 := PlayGame(446, 71522)
	fmt.Println("result", g3.max())
	fmt.Println(time.Since(start))

	start = time.Now()
	g4 := PlayGame(446, 7152200)
	fmt.Println("result", g4.max())
	fmt.Println(time.Since(start))
}
