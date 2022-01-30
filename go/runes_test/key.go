package main

import (
    "strings"
    "io/ioutil"
    "gopkg.in/yaml.v2"
    "github.com/gdamore/tcell/v2"
)

var (
    K9sBindingPath = "/home/ntj/.config/k9s/bindings.yml"
)

// Maps from key literals to tcell.Key enums.
// Not only in go, parsing enums from a yaml is usually unsupported.
// The following is the exhaustive list of non-rune-keys supported by tcell, 
// others are handled as plain runes.
var (
    nonRunes = map[string]tcell.Key {
        "Rune": tcell.KeyRune,
        "Up": tcell.KeyUp,
        "Down": tcell.KeyDown,
        "Right": tcell.KeyRight,
        "Left": tcell.KeyLeft,
        "UpLeft": tcell.KeyUpLeft,
        "UpRight": tcell.KeyUpRight,
        "DownLeft": tcell.KeyDownLeft,
        "DownRight": tcell.KeyDownRight,
        "Center": tcell.KeyCenter,
        "PgUp": tcell.KeyPgUp,
        "PgDn": tcell.KeyPgDn,
        "Home": tcell.KeyHome,
        "End": tcell.KeyEnd,
        "Insert": tcell.KeyInsert,
        "Delete": tcell.KeyDelete,
        "Help": tcell.KeyHelp,
        "Exit": tcell.KeyExit,
        "Clear": tcell.KeyClear,
        "Cancel": tcell.KeyCancel,
        "Print": tcell.KeyPrint,
        "Pause": tcell.KeyPause,
        "Backtab": tcell.KeyBacktab,
        "F1": tcell.KeyF1,
        "F2": tcell.KeyF2,
        "F3": tcell.KeyF3,
        "F4": tcell.KeyF4,
        "F5": tcell.KeyF5,
        "F6": tcell.KeyF6,
        "F7": tcell.KeyF7,
        "F8": tcell.KeyF8,
        "F9": tcell.KeyF9,
        "F10": tcell.KeyF10,
        "F11": tcell.KeyF11,
        "F12": tcell.KeyF12,
        "F13": tcell.KeyF13,
        "F14": tcell.KeyF14,
        "F15": tcell.KeyF15,
        "F16": tcell.KeyF16,
        "F17": tcell.KeyF17,
        "F18": tcell.KeyF18,
        "F19": tcell.KeyF19,
        "F20": tcell.KeyF20,
        "F21": tcell.KeyF21,
        "F22": tcell.KeyF22,
        "F23": tcell.KeyF23,
        "F24": tcell.KeyF24,
        "F25": tcell.KeyF25,
        "F26": tcell.KeyF26,
        "F27": tcell.KeyF27,
        "F28": tcell.KeyF28,
        "F29": tcell.KeyF29,
        "F30": tcell.KeyF30,
        "F31": tcell.KeyF31,
        "F32": tcell.KeyF32,
        "F33": tcell.KeyF33,
        "F34": tcell.KeyF34,
        "F35": tcell.KeyF35,
        "F36": tcell.KeyF36,
        "F37": tcell.KeyF37,
        "F38": tcell.KeyF38,
        "F39": tcell.KeyF39,
        "F40": tcell.KeyF40,
        "F41": tcell.KeyF41,
        "F42": tcell.KeyF42,
        "F43": tcell.KeyF43,
        "F44": tcell.KeyF44,
        "F45": tcell.KeyF45,
        "F46": tcell.KeyF46,
        "F47": tcell.KeyF47,
        "F48": tcell.KeyF48,
        "F49": tcell.KeyF49,
        "F50": tcell.KeyF50,
        "F51": tcell.KeyF51,
        "F52": tcell.KeyF52,
        "F53": tcell.KeyF53,
        "F54": tcell.KeyF54,
        "F55": tcell.KeyF55,
        "F56": tcell.KeyF56,
        "F57": tcell.KeyF57,
        "F58": tcell.KeyF58,
        "F59": tcell.KeyF59,
        "F60": tcell.KeyF60,
        "F61": tcell.KeyF61,
        "F62": tcell.KeyF62,
        "F63": tcell.KeyF63,
        "F64": tcell.KeyF64,
        "Ctrl+Space": tcell.KeyCtrlSpace,
        "Ctrl+A": tcell.KeyCtrlA,
        "Ctrl+B": tcell.KeyCtrlB,
        "Ctrl+C": tcell.KeyCtrlC,
        "Ctrl+D": tcell.KeyCtrlD,
        "Ctrl+E": tcell.KeyCtrlE,
        "Ctrl+F": tcell.KeyCtrlF,
        "Ctrl+G": tcell.KeyCtrlG,
        "Ctrl+H": tcell.KeyCtrlH,
        "Ctrl+I": tcell.KeyCtrlI,
        "Ctrl+J": tcell.KeyCtrlJ,
        "Ctrl+K": tcell.KeyCtrlK,
        "Ctrl+L": tcell.KeyCtrlL,
        "Ctrl+M": tcell.KeyCtrlM,
        "Ctrl+N": tcell.KeyCtrlN,
        "Ctrl+O": tcell.KeyCtrlO,
        "Ctrl+P": tcell.KeyCtrlP,
        "Ctrl+Q": tcell.KeyCtrlQ,
        "Ctrl+R": tcell.KeyCtrlR,
        "Ctrl+S": tcell.KeyCtrlS,
        "Ctrl+T": tcell.KeyCtrlT,
        "Ctrl+U": tcell.KeyCtrlU,
        "Ctrl+V": tcell.KeyCtrlV,
        "Ctrl+W": tcell.KeyCtrlW,
        "Ctrl+X": tcell.KeyCtrlX,
        "Ctrl+Y": tcell.KeyCtrlY,
        "Ctrl+Z": tcell.KeyCtrlZ,
        "Ctrl+LeftSq": tcell.KeyCtrlLeftSq,
        "Ctrl+Backslash": tcell.KeyCtrlBackslash,
        "Ctrl+RightSq": tcell.KeyCtrlRightSq,
        "Ctrl+Carat": tcell.KeyCtrlCarat,
        "Ctrl+Underscore": tcell.KeyCtrlUnderscore,
        "NUL": tcell.KeyNUL,
        "SOH": tcell.KeySOH,
        "STX": tcell.KeySTX,
        "ETX": tcell.KeyETX,
        "EOT": tcell.KeyEOT,
        "ENQ": tcell.KeyENQ,
        "ACK": tcell.KeyACK,
        "BEL": tcell.KeyBEL,
        "BS": tcell.KeyBS,
        "TAB": tcell.KeyTAB,
        "LF": tcell.KeyLF,
        "VT": tcell.KeyVT,
        "FF": tcell.KeyFF,
        "CR": tcell.KeyCR,
        "SO": tcell.KeySO,
        "SI": tcell.KeySI,
        "DLE": tcell.KeyDLE,
        "DC1": tcell.KeyDC1,
        "DC2": tcell.KeyDC2,
        "DC3": tcell.KeyDC3,
        "DC4": tcell.KeyDC4,
        "NAK": tcell.KeyNAK,
        "SYN": tcell.KeySYN,
        "ETB": tcell.KeyETB,
        "CAN": tcell.KeyCAN,
        "EM": tcell.KeyEM,
        "SUB": tcell.KeySUB,
        "ESC": tcell.KeyESC,
        "FS": tcell.KeyFS,
        "GS": tcell.KeyGS,
        "RS": tcell.KeyRS,
        "US": tcell.KeyUS,
        "DEL": tcell.KeyDEL,
    }
)

type Key struct {
	KeyEvent *tcell.EventKey
    KeyLiteral string `yaml:"key"`
	Description string `yaml:"description"`
	Command string `yaml:"command"`
}

type KeyGroup struct {
    Keys map[string]Key `yaml:"bindings"`
}

func (kg *KeyGroup) Load() error {
    f, err := ioutil.ReadFile(K9sBindingPath)
    if err != nil {
        return err
    }

    err = yaml.Unmarshal(f, kg)
    if err != nil {
        return err
    }

    return nil
}

func (k *Key) Parse() {
    // Modifiers in effect
    // * Shift: Usually just capitalizes the letters or invokes other characters
    // * Ctrl: Can be used with most keys, case-sensitive, incompatible with non-ascii keys
    // * Alt: Works the same as Ctrl
    // * Mod: tcell's current implementation intentionally doesn't use it
    //
    //https://pkg.go.dev/github.com/gdamore/tcell/v2#ModMask
    /*
    Config format:
        bindings:
          <commandGroup>:
            - key: [Ctrl+][Alt+]<Key>
              description: Description of the command
              command: The identifier of the command
    */
    splitKey := strings.Split(k.KeyLiteral, "+")

    var modMask tcell.ModMask = 0
    for _, v := range splitKey {
        if v == "Ctrl" {
            modMask += tcell.ModCtrl
        }
        if v == "Alt" {
            modMask += tcell.ModAlt
        }
    }

    if key, found := nonRunes[k.KeyLiteral]; found {
        k.KeyEvent = tcell.NewEventKey(tcell.Key(key), rune(key), modMask)
    } else {
        k.KeyEvent = tcell.NewEventKey(tcell.Key(256), []rune(splitKey[len(splitKey)-1])[0], modMask)
    }
}

func (kg *KeyGroup) Parse() {
    for k, v := range kg.Keys {
        v.Parse()
        kg.Keys[k] = v
    }
}
