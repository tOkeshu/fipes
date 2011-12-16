var tnetstrings = {
    parsePayload: function(data) {
        if (!data) {
            throw "Invalid data to parse, it's empty.";
        }
        var idx = data.indexOf(":");
        if (idx === -1) {
            throw "Missing netstring delimiter";
        }

        var length = parseInt(data.substr(0, idx), 10);
        var payload = data.substr(idx + 1, length);
        if (payload.length !== length) {
            throw "Data is wrong length: " + length + " vs " + payload.length;
        }

        var type = data.substr(idx + length + 1, 1);
        var extra = data.substr(idx + length + 2);
        if (!type) {
            throw "No payload type: " + extra;
        }
        return {payload: payload, type: type, extra: extra};
    },


    parseList: function(data) {
        if (!data) {
            return [];
        }
        var result = [];
        var p = this.parse(data);
        result[result.length] = p.value;
        while(p.extra) {
            p = this.parse(p.extra);
            result[result.length] = p.value;
        }
        return result;
    },

    parsePair: function(data) {
        var pk = this.parse(data);
        if (!pk.extra) {
            throw "Unbalanced dictionary store.";
        }
        var pv = this.parse(pk.extra);
        if (!pk.value) {
            throw "Got an invalid value, null not allowed.";
        }
        return {key: pk.value, value: pv.value, extra: pv.extra};
    },

    parseDict: function(data) {
        if (!data) {
            return {};
        }
        var kv = this.parsePair(data);
        var result = {};
        result[kv.key] = kv.value;
        while (kv.extra) {
            kv = this.parsePair(kv.extra);
            result[kv.key] = kv.value;
        }
        return result;
    },

    dumpObject: function(data) {
        var i, type, result = [];

        if (data === null) {
            return '0:~';
        } else if (Object.prototype.toString.apply(data) === '[object Array]') {
            type = ']';
            for (i = 0; i < data.length; i++) {
                result.push(this.dump(data[i]));
            }
        } else {
            type = '}';
            for (i in data) {
                if (data.hasOwnProperty(i)) {
                    result.push(this.dump(i), this.dump(data[i]));
                }
            }
        }

        var payload = result.join('');
        return payload.length + ':' + payload + type;
    },

    parse: function(data) {
        var value;
        var p = this.parsePayload(data);
        switch (p.type) {
        case '#':
            value = parseInt(p.payload, 10);
            break;
        case '^':
            value = parseFloat(p.payload);
            break;
        case '!':
            value = p.payload === 'true';
            break;
        case '~':
            if (p.payload.length !== 0)
                throw "Payload must be 0 length for null.";
            value = null;
            break;
        case ',':
            value = p.payload;
            break;
        case '}':
            value = this.parseDict(p.payload);
            break;
        case ']':
            value = this.parseList(p.payload);
            break;
        default:
            throw "Invalid payload type: " + p.type;
        }
        return {value: value, extra: p.extra};
    },

    dump: function(data) {
        switch (typeof data) {
        case 'number':
            // return null for infinite numbers
            if (!isFinite(data))
                return this.dump(null);
            var out = data.toString();
            return out.length + ':' + out + (~~data === data ? '#' : '^');
        case 'boolean':
            var out = data.toString();
            return out.length + ':' + out + '!';
        case 'string':
            return data.length + ':' + data + ',';
        case 'object':
            // object in js could be dict, list, null
            return this.dumpObject(data);
        }
    }
};

if (typeof module !== 'undefined' && module.exports) {
    module.exports = tnetstrings;
}
