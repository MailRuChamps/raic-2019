package stream

import "io"
import "encoding/binary"

func ReadBool(reader io.Reader) bool {
	var value bool
	err := binary.Read(reader, binary.LittleEndian, &value)
	if err != nil {
		panic(err)
	}
	return value
}

func ReadInt32(reader io.Reader) int32 {
	var value int32
	err := binary.Read(reader, binary.LittleEndian, &value)
	if err != nil {
		panic(err)
	}
	return value
}

func ReadInt64(reader io.Reader) int64 {
	var value int64
	err := binary.Read(reader, binary.LittleEndian, &value)
	if err != nil {
		panic(err)
	}
	return value
}

func ReadFloat32(reader io.Reader) float32 {
	var value float32
	err := binary.Read(reader, binary.LittleEndian, &value)
	if err != nil {
		panic(err)
	}
	return value
}

func ReadFloat64(reader io.Reader) float64 {
	var value float64
	err := binary.Read(reader, binary.LittleEndian, &value)
	if err != nil {
		panic(err)
	}
	return value
}

func ReadString(reader io.Reader) string {
	bytes := make([]byte, ReadInt32(reader))
	_, err := io.ReadFull(reader, bytes)
	if err != nil {
		panic(err)
	}
	return string(bytes)
}

func WriteBool(writer io.Writer, value bool) {
	err := binary.Write(writer, binary.LittleEndian, value)
	if err != nil {
		panic(err)
	}
}

func WriteInt32(writer io.Writer, value int32) {
	err := binary.Write(writer, binary.LittleEndian, value)
	if err != nil {
		panic(err)
	}
}

func WriteInt64(writer io.Writer, value int64) {
	err := binary.Write(writer, binary.LittleEndian, value)
	if err != nil {
		panic(err)
	}
}

func WriteFloat32(writer io.Writer, value float32) {
	err := binary.Write(writer, binary.LittleEndian, value)
	if err != nil {
		panic(err)
	}
}

func WriteFloat64(writer io.Writer, value float64) {
	err := binary.Write(writer, binary.LittleEndian, value)
	if err != nil {
		panic(err)
	}
}

func WriteString(writer io.Writer, value string) {
	bytes := []byte(value)
	WriteInt32(writer, int32(len(bytes)))
	_, err := writer.Write(bytes)
	if err != nil {
		panic(err)
	}
}