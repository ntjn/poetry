// An example to see how tcell handles keypresses
// and a PoC that implements loading keybindings from a configfile.

package main

import (
    "fmt"
    "os"
    "time"
	"log"
	"github.com/mattn/go-runewidth"
    "github.com/gdamore/tcell/v2"
    "github.com/gdamore/tcell/v2/encoding"
)

func emitStr(s tcell.Screen, x, y int, style tcell.Style, str string) {
	for _, c := range str {
		var comb []rune
		w := runewidth.RuneWidth(c)
		if w == 0 {
			comb = []rune{c}
			c = ' '
			w = 1
		}
		s.SetContent(x, y, c, comb, style)
		x += w
	}
}

// TODO: Move this to a unittest
// TODO: Make this less spaghetti
func testKeyLoad() {
	file, err := os.OpenFile("test.log", os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0666)
    if err != nil {
        log.Fatal(err)
    }
	log.SetOutput(file)

    kg := new(KeyGroup)
    err = kg.Load()
	if err != nil {
		log.Printf("Error: %s\n", err)
	} else {
		log.Printf("The loaded config contains: \n%s\n", kg)
		k := kg.Keys["sofun"]
		(&k).Parse()
		kg.Keys["sofun"] = k
		log.Printf("\nKey: %s\nRune: %s\nModifiers: %s\nName: %s\n",
			k.KeyEvent.Key(), k.KeyEvent.Rune(), k.KeyEvent.Modifiers(), k.KeyEvent.Name())
	}
}

func showKeyEvent(x int, y int, screen tcell.Screen, style tcell.Style, ev *tcell.EventKey) {
	emitStr(screen, x, y, style, fmt.Sprintf("Key: %s", ev.Key()))
	emitStr(screen, x, y+1, style, fmt.Sprintf("Rune: %s", ev.Rune()))
	emitStr(screen, x, y+2, style, fmt.Sprintf("Modifiers: %s", ev.Modifiers()))
	emitStr(screen, x, y+3, style, fmt.Sprintf("Name: %s", ev.Name()))
}

func showHelp(x int, y int, screen tcell.Screen, style tcell.Style) {
    help := []string {
        "Press a button to see its values",
        "b: Show bindings",
        "Esc: Exit",
    }

	emitStr(screen, x, y, style, "Help:")
    for i := 0; i < 3; i++ {
	    emitStr(screen, x+4, y+1+i, style, help[i])
    }
}

func showBindings(kg *KeyGroup, screen tcell.Screen, style tcell.Style) {
	emitStr(screen, 5, 13, style, fmt.Sprintf("%s", kg))
}

func main() {
    s, e :=  tcell.NewScreen()

    if e != nil {
        fmt.Fprintf(os.Stderr, "%v\n", e)
        os.Exit(1)
    }
    if e := s.Init(); e != nil {
        fmt.Fprintf(os.Stderr, "%v\n", e)
        os.Exit(1)
    }

    encoding.Register()
    defStyle := tcell.StyleDefault.Background(tcell.ColorReset).Foreground(tcell.ColorReset)

    s.SetStyle(defStyle)
    s.EnableMouse()
    s.EnablePaste()

	testKeyLoad()

	kg := new(KeyGroup)
    err := kg.Load()
	if err != nil {
		log.Printf("Error: %s\n", err)
	}
	kg.Parse()

	bindingsShown := false

	s.Show()

    events := make(chan tcell.Event)

    go func() {
        for {
            event := s.PollEvent()
            events <- event
        }
    }()
    go func() {
        for {
            ev := <-events
            s.Clear()
            showHelp(5, 8, s, defStyle)
            switch ev := ev.(type) {
                case *tcell.EventKey:
                    if ev.Key() == tcell.KeyEscape {
                        s.Fini()
                        os.Exit(0)
                    } else {
						showKeyEvent(5, 3, s, defStyle, ev)
                    }

					if ev.Rune() == 'b' {
                        bindingsShown = !bindingsShown
                    }
            }
            if bindingsShown {
                showBindings(kg, s, defStyle)
            }
            s.Sync()
            s.Show()
        }
    }()

    defer s.Fini()
    t := time.NewTicker(time.Second)
    for {
        <-t.C
    }
}
