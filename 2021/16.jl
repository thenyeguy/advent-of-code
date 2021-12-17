module Day16

#
# Data loading:
function load_data()
    return read(joinpath(@__DIR__, "16.txt"), String)
end

#
# Common code:
struct Packet
    version::Int
    type_::Int
    payload::Union{Int,Array{Packet}}
end

function to_binary(hex)
    parse_hex(d) = parse(Int, d, base=16)
    binary_str(n) = string(n, base=2, pad=4)
    return split(hex, "") .|> parse_hex .|> binary_str |> prod
end

function from_binary(str)
    parse(Int, str, base=2)
end

function parse_packet(packet)
    version = from_binary(packet[1:3])
    type_ = from_binary(packet[4:6])

    if type_ == 4
        data = ""
        for i in 7:5:typemax(Int)
            data *= packet[i+1:i+4]
            if packet[i] == '0'
                value = from_binary(data)
                return Packet(version, type_, value), packet[i+5:end]
            end
        end
    else
        length_type = from_binary(packet[7])
        packets = Packet[]
        rest = ""
        if length_type == 1
            num_subpackets = from_binary(packet[8:18])
            rest = packet[19:end]
            for i in 1:num_subpackets
                parsed, rest = parse_packet(rest)
                push!(packets, parsed)
            end
        else
            subpacket_length = from_binary(packet[8:22])
            data = packet[23:23+subpacket_length-1]
            rest = packet[23+subpacket_length:end]
            while !isempty(data)
                parsed, data = parse_packet(data)
                push!(packets, parsed)
            end
        end
        return Packet(version, type_, packets), rest
    end
end

#
# Part A:
function sum_versions(packet)
    versions = packet.version
    if isa(packet.payload, Array{Packet})
        versions += packet.payload .|> sum_versions |> sum
    end
    return versions
end

function part_a(data)
    return data |> to_binary |> parse_packet |> first |> sum_versions
end

#
# Part B:
function evaluate_packet(packet)
    if packet.type_ == 4
        return packet.payload
    end

    values = packet.payload .|> evaluate_packet
    if packet.type_ == 0     return sum(values)
    elseif packet.type_ == 1 return prod(values)
    elseif packet.type_ == 2 return minimum(values)
    elseif packet.type_ == 3 return maximum(values)
    elseif packet.type_ == 5 return values[1] > values[2]
    elseif packet.type_ == 6 return values[1] < values[2]
    elseif packet.type_ == 7 return values[1] == values[2]
    end
end

function part_b(data)
    return data |> to_binary |> parse_packet |> first |> evaluate_packet
end

function main()
    data = load_data()
    println("Part A: ", part_a(data))
    println("Part B: ", part_b(data))
end

end # module Day16

if abspath(PROGRAM_FILE) == @__FILE__
    Day16.main()
end
