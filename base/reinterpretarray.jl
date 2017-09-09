"""
Gives a reinterpreted view (of element type T) of the underlying array (of element type S).
If the size of `T` differs from the size of `S`, the array will be compressed/expanded in
the first dimension.
"""
struct ReinterpretArray{T,N,S,A<:AbstractArray{S, N}} <: AbstractArray{T, N}
    parent::A
    function reinterpret(::Type{T}, a::A) where {T,N,S,A<:AbstractArray{S, N}}
        function throwbits(::Type{S}, ::Type{T}, ::Type{U}) where {S,T,U}
            @_noinline_meta
            throw(ArgumentError("cannot reinterpret `$(S)` `$(T)`, type `$(U)` is not a bits type"))
        end
        function throwsize0(::Type{S}, ::Type{T})
            @_noinline_meta
            throw(ArgumentError("cannot reinterpret a zero-dimensional `$(S)` array to `$(T)` which is of a different size"))
        end
        isbits(T) || throwbits(S, T, T)
        isbits(S) || throwbits(S, T, S)
        (N != 0 || sizeof(T) == sizeof(S)) || throwsize0(S, T)
        new{T, N, S, A}(a)
    end
end

eltype(a::ReinterpretArray{T}) where {T} = T
function size(a::ReinterpretArray{T,N,S} where {N}) where {T,S}
    psize = size(a.parent)
    if sizeof(T) > sizeof(S)
        size1 = div(psize[1], div(sizeof(T), sizeof(S)))
    else
        size1 = psize[1] * div(sizeof(S), sizeof(T))
    end
    tuple(size1, tail(psize)...)
end

unsafe_convert(::Type{Ptr{T}}, a::ReinterpretArray{T,N,S} where N) where {T,S} = Ptr{T}(unsafe_convert(Ptr{S},a.parent))

@inline @propagate_inbounds getindex(a::ReinterpretArray{T,0}) where {T} = reinterpret(T, a.parent[])
@inline @propagate_inbounds getindex(a::ReinterpretArray) = a[1]

@inline @propagate_inbounds function getindex(a::ReinterpretArray{T,N,S}, inds::Vararg{Int, N}) where {T,N,S}
    if sizeof(T) == sizeof(S)
        return reinterpret(T, a.parent[inds...])
    elseif sizeof(T) > sizeof(S)
        nels = div(sizeof(T), sizeof(S))
        ind_off = (inds[1]-1) * nels
        o = Ref{T}()
        @gc_preserve o begin
            optr = unsafe_convert(Ref{T}, o)
            for i = 1:nels
                unsafe_store!(Ptr{S}(optr), a.parent[ind_off + i, tail(inds)...], i)
            end
        end
        return o[]
    else
        ind, sub = divrem(inds[1]-1, div(sizeof(S), sizeof(T)))
        r = Ref{S}(a.parent[1+ind, tail(inds)...])
        @gc_preserve r begin
            rptr = unsafe_convert(Ref{S}, r)
            ret = unsafe_load(Ptr{T}(rptr), sub+1)
        end
        return ret
    end
end

@inline @propagate_inbounds setindex!(a::ReinterpretArray{T,0,S} where T, v) where {S} = (a.parent[] = reinterpret(S, v))
@inline @propagate_inbounds setindex!(a::ReinterpretArray, v) = (a[1] = v)

@inline @propagate_inbounds function setindex!(a::ReinterpretArray{T,N,S}, v, inds::Vararg{Int, N}) where {T,N,S}
    v = convert(T, v)::T
    if sizeof(T) == sizeof(S)
        return setindex!(a, reinterpret(S, v), inds...)
    elseif sizeof(T) > sizeof(S)
        nels = div(sizeof(T), sizeof(S))
        ind_off = (inds[1]-1) * nels
        o = Ref{T}(v)
        @gc_preserve o begin
            optr = unsafe_convert(Ref{T}, o)
            for i = 1:nels
                a.parent[ind_off + i, tail(inds)...] = unsafe_load(Ptr{S}(optr), i)
            end
        end
    else
        ind, sub = divrem(inds[1]-1, div(sizeof(S), sizeof(T)))
        r = Ref{S}(a.parent[1+ind, tail(inds)...])
        @gc_preserve r begin
            rptr = unsafe_convert(Ref{S}, r)
            unsafe_store!(Ptr{T}(rptr), v, sub+1)
        end
        a.parent[1+ind] = r[]
    end
    return a
end
