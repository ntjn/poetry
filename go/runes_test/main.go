package main

import (
    "fmt"
    "os"
    "time"
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
            switch ev := ev.(type) {
                case *tcell.EventKey:
                    if ev.Key() == tcell.KeyEscape {
                        s.Fini()
                        os.Exit(0)
                    } else {
                        s.Clear()
                        emitStr(s, 0, 0, defStyle, fmt.Sprintf("%s", ev.Key()))
                        emitStr(s, 0, 1, defStyle, fmt.Sprintf("%s", ev.Rune()))
                        emitStr(s, 0, 2, defStyle, fmt.Sprintf("%s", ev.Name()))
                    }
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
