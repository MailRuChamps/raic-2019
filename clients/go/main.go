package main

import (
    . "aicup2019/model"
    . "aicup2019/stream"
    "os"
    "strconv"
    "net"
    "bufio"
)

type Runner struct {
	conn net.Conn
	reader *bufio.Reader
    writer *bufio.Writer
}

func NewRunner(host string, port uint16, token string) Runner {
    conn, err := net.Dial("tcp", host + ":" + strconv.Itoa(int(port)))
    if err != nil {
        panic(err)
    }
    writer := bufio.NewWriter(conn)
    WriteString(writer, token)
    err = writer.Flush()
    if err != nil {
        panic(err)
    }
    return Runner {
        conn: conn,
        reader: bufio.NewReader(conn),
        writer: writer,
    }
}

func (runner Runner) Run() {
    myStrategy := NewMyStrategy()
    debug := Debug {
        Writer: runner.writer,
    }
    for {
        message := ReadServerMessageGame(runner.reader)
        if message.PlayerView == nil {
            break
        }
        playerView := *message.PlayerView
        actions := make(map[int32]UnitAction)
        for _, unit := range playerView.Game.Units {
            if (unit.PlayerId == playerView.MyId) {
                actions[unit.Id] = myStrategy.getAction(unit, playerView.Game, debug)
            }
        }
        PlayerMessageGameActionMessage {
            Action: actions,
        }.Write(runner.writer)
        err := runner.writer.Flush()
        if err != nil {
            panic(err)
        }
    }
}

func main() {
    var host string
    if len(os.Args) < 2 {
        host = "localhost"
    } else {
        host = os.Args[1]
    }
    var port uint16
    if len(os.Args) < 3 {
        port = 31001
    } else {
        portInt, err := strconv.Atoi(os.Args[2])
        port = uint16(portInt)
        if err != nil {
            panic(err)
        }
    }
    var token string
    if len(os.Args) < 4 {
        token = "0000000000000000"
    } else {
        token = os.Args[3]
    }
    
    runner := NewRunner(host, port, token)
    runner.Run()
}