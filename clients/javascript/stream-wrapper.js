'use strict';

const os = require('os');

const BOOL_SIZE = 1;
const INT_SIZE = 4;
const LONG_SIZE = 8;
const FLOAT_SIZE = 4;
const DOUBLE_SIZE = 8;

class StreamWrapper {
    constructor (socket) {
        this.socket = socket;
        this.isLittleEndianMachine = (os.endianness() === 'LE');   
    }

    close () {
        this.socket.destroy();
    }

    // Reading primitives

    _read (size, timeout = 10000) {
        // console.trace('Reading ' + size);
        const socket = this.socket;
        return new Promise(function (resolve, reject) {
            let timer;
            const responseHandler = function () {
                const data = socket.read(size);
                clearTimeout(timer);
                resolve(data);
            };

            socket.once('readable', responseHandler);

            timer = setTimeout(() => {
                socket.removeListener('readable', responseHandler);
                reject(new Error('timeout waiting for data'));
            }, timeout);
        }).catch(function (error) {
            console.log('Error while reading data', error);
        });
    }

    async readBool () {
        const buffer = await this._read(BOOL_SIZE);
        return !!buffer.readInt8();
    }

    async readInt () {
        const buffer = await this._read(INT_SIZE);
        if (this.isLittleEndianMachine) {
            return parseInt(buffer.readIntLE());
        }
        return parseInt(buffer.readIntBE());
    }

    async readLong () {
        const buffer = await this._read(LONG_SIZE);
        if (this.isLittleEndianMachine) {
            return parseInt(buffer.readBigInt64LE());
        }
        return parseInt(buffer.readBigInt64BE());
    }

    async readFloat () {
        const buffer = await this._read(FLOAT_SIZE);
        if (this.isLittleEndianMachine) {
            return parseFloat(buffer.readFloatLE());
        }
        return parseFloat(buffer.readFloatBE());
    }

    async readDouble () {
        const buffer = await this._read(DOUBLE_SIZE);
        if (this.isLittleEndianMachine) {
            return parseFloat(buffer.readDoubleLE());
        }
        return parseFloat(buffer.readDoubleBE());
    }

    async readString () {
        const length = this.readInt();
        const buffer = await this._read(length);
        const result = buffer.toString();
        if (result.length !== length) {
            throw new Error('Unexpected EOF');
        }
        return result;
    }

    // Writing primitives

    _write (data) {
        const socket = this.socket;
        return new Promise(function (resolve, reject) {
            socket.write(data, 'utf8', function (error) {
                if (error) {
                    return reject(error);
                }
                resolve(data);
            });
        }).catch(function (error) {
            console.log('Error while writing data', error);
        });
    }

    async writeBool (value) {
        const buffer = Buffer.alloc(BOOL_SIZE);
        buffer.writeInt8(value);
        return await this._write(buffer);
    }

    async writeInt (value) {
        const buffer = Buffer.alloc(INT_SIZE);
        if (this.isLittleEndianMachine) {
            buffer.writeInt32LE(value);
        } else {
            buffer.writeInt32BE(value);
        }
        return await this._write(buffer);
    }

    async writeLong (value) {
        const buffer = Buffer.alloc(LONG_SIZE);
        if (this.isLittleEndianMachine) {
            buffer.writeBigInt64LE(value);
        } else {
            buffer.writeBigInt64BE(value);
        }
        return await this._write(buffer);
    }

    async writeFloat (value) {
        const buffer = Buffer.alloc(FLOAT_SIZE);
        if (this.isLittleEndianMachine) {
            buffer.writeFloatLE(value);
        } else {
            buffer.writeFloatBE(value);
        }
        return await this._write(buffer);
    }

    async writeDouble (value) {
        const buffer = Buffer.alloc(DOUBLE_SIZE);
        if (this.isLittleEndianMachine) {
            buffer.writeDoubleLE(value);
        } else {
            buffer.writeDoubleBE(value);
        }
        return await this._write(buffer);
    }

    async writeString (value) {
        this.writeInt(value.length);
        return await this._write(value, 'utf8');
    }
}

module.exports.StreamWrapper = StreamWrapper;
