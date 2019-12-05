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
        this.data = Buffer.alloc(0);
        this.needAmount = null;
        this.resolve = null;
        const _this = this;
        this.socket.on('data', function(data) { _this.dataHandler(data) });
        this.isLittleEndianMachine = (os.endianness() === 'LE');
    }

    dataHandler(data) {
        this.data = Buffer.concat([this.data, data]);
        this.update();
    }

    update() {
        if (this.needAmount === null || this.needAmount > this.data.length) {
            return;
        }
        const data = this.data.slice(0, this.needAmount);
        this.data = this.data.slice(this.needAmount);
        this.needAmount = null;
        this.resolve(data);
        this.update();
    }

    close () {
        this.socket.destroy();
    }

    // Reading primitives

    _read (size) {
        const _this = this;
        return new Promise(function (resolve, reject) {
            _this.needAmount = size;
            _this.resolve = resolve;
            _this.update();
        }).catch(function (error) {
            throw new Error('Error while reading data: ' + error.message);
        });
    }

    async readBool () {
        const buffer = await this._read(BOOL_SIZE);
        return !!buffer.readInt8();
    }

    async readInt () {
        const buffer = await this._read(INT_SIZE);
        if (this.isLittleEndianMachine) {
            return parseInt(buffer.readInt32LE(0, INT_SIZE));
        }
        return parseInt(buffer.readInt32BE(0, INT_SIZE));
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
            return buffer.readFloatLE();
        }
        return buffer.readFloatBE();
    }

    async readDouble () {
        const buffer = await this._read(DOUBLE_SIZE);
        if (this.isLittleEndianMachine) {
            return buffer.readDoubleLE();
        }
        return buffer.readDoubleBE();
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
                resolve(true);
            });
        }).catch(function (error) {
            console.log('Error while writing data ' + error.message);
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