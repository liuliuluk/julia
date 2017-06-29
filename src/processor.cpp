// This file is a part of Julia. License is MIT: https://julialang.org/license

// Processor feature detection

#include "processor.h"

#include "julia.h"
#include "julia_internal.h"

#include <map>
#include <algorithm>

#include "julia_assert.h"

// CPU target string is a list of strings separated by `;` each string starts with a CPU
// or architecture name and followed by an optional list of features separated by `,`.
// A "generic" or empty CPU name means the basic required feature set of the target ISA
// which is at least the architecture the C/C++ runtime is compiled with.

// CPU dispatch needs to determine the version to be used by the sysimg as well as
// the target and feature used by the JIT. Currently the only limitation on JIT target
// and feature is matching register size between the sysimg and JIT so that SIMD vectors
// can be passed correctly. This means disabling AVX and AVX2 if AVX was not enabled
// in sysimg and disabling AVX512 if it was not enabled in sysimg.
// This also possibly means that SVE needs to be disabled on AArch64 if sysimg doesn't have it
// enabled.

// CPU dispatch starts by first deciding the max feature set and CPU requested for JIT.
// This is the host or the target specified on the command line with features unavailable
// on the host disabled. All sysimg targets that require features not available in this set
// will be ignored.

// The next step is matching CPU name.
// If exact name match with compatible feature set exists, all versions without name match
// are ignored.
// This step will query LLVM first so it can accept CPU names that is recognized by LLVM but
// not by us (yet) when LLVM is enabled.

// If there are still more than one candidates, a feature match is performed.
// The ones with the largest register size will be used
// (i.e. AVX512 > AVX2/AVX > SSE, SVE > ASIMD). If there's a tie, the one with the most features
// enabled will be used. If there's still a tie the one that appears later in the list will be
// used. (i.e. the order in the version list is significant in this case).

// Features that are not recognized will be passed to LLVM directly during codegen
// but ignored otherwise.

// Two special features are supported:
// 1. `clone_all`
//
//     This forces the target to have all functions in sysimg cloned.
//     When used in negative form (i.e. `-clone_all`), this disables full clone that's
//     enabled by default for certain targets.
//
// 2. `base([0-9]*)`
//
//     This specifies the (0-based) base target index. The base target is the target
//     that the current target is based on, i.e. the functions that are not being cloned
//     will use the version in the base target. This option causes the base target to be
//     fully cloned (as if `clone_all` is specified for it) if it is not the default target (0).
//     The index can only be smaller than the current index.

namespace {

// Helper functions to test/set feature bits

template<typename T1, typename T2, typename T3>
static inline bool test_bits(T1 v, T2 mask, T3 test)
{
    return T3(v & mask) == test;
}

template<typename T1, typename T2>
static inline bool test_bits(T1 v, T2 mask)
{
    return test_bits(v, mask, mask);
}

template<typename T1, typename T2>
static inline bool test_nbit(const T1 &bits, T2 _bitidx)
{
    auto bitidx = static_cast<uint32_t>(_bitidx);
    auto u32idx = bitidx / 32;
    auto bit = bitidx % 32;
    return (bits[u32idx] & (1 << bit)) != 0;
}

template<typename T>
static inline void unset_bits(T &bits)
{
    (void)bits;
}

template<typename T, typename T1, typename... Rest>
static inline void unset_bits(T &bits, T1 _bitidx, Rest... rest)
{
    auto bitidx = static_cast<uint32_t>(_bitidx);
    auto u32idx = bitidx / 32;
    auto bit = bitidx % 32;
    bits[u32idx] = bits[u32idx] & ~uint32_t(1 << bit);
    unset_bits(bits, rest...);
}

template<typename T, typename T1>
static inline void set_bit(T &bits, T1 _bitidx, bool val)
{
    auto bitidx = static_cast<uint32_t>(_bitidx);
    auto u32idx = bitidx / 32;
    auto bit = bitidx % 32;
    if (val) {
        bits[u32idx] = bits[u32idx] | uint32_t(1 << bit);
    }
    else {
        bits[u32idx] = bits[u32idx] & ~uint32_t(1 << bit);
    }
}

// Helper functions to create feature masks

// This can be `std::array<uint32_t,n>` on C++14
template<size_t n>
struct FeatureList {
    uint32_t eles[n];
    uint32_t &operator[](size_t pos)
    {
        return eles[pos];
    }
    constexpr const uint32_t &operator[](size_t pos) const
    {
        return eles[pos];
    }
    inline int nbits() const
    {
        int cnt = 0;
        for (size_t i = 0; i < n; i++) {
#ifdef __GNUC__
            cnt += __builtin_popcount(eles[i]);
#else
            uint32_t v = eles[i];
            v = v - ((v >> 1) & 0x55555555);
            v = (v & 0x33333333) + ((v >> 2) & 0x33333333);
            cnt += ((v + (v >> 4) & 0xF0F0F0F) * 0x1010101) >> 24;
#endif
        }
        return cnt;
    }
    inline bool empty() const
    {
        for (size_t i = 0; i < n; i++) {
            if (eles[i]) {
                return false;
            }
        }
        return true;
    }
};

template<size_t n>
static inline bool features_le(const FeatureList<n> &a, const FeatureList<n> &b)
{
    for (size_t i = 0; i < n; i++) {
        if (a[i] & ~b[i]) {
            return false;
        }
    }
    return true;
}

static inline constexpr uint32_t add_feature_mask_u32(uint32_t mask, uint32_t u32idx)
{
    return mask;
}

template<typename T, typename... Rest>
static inline constexpr uint32_t add_feature_mask_u32(uint32_t mask, uint32_t u32idx,
                                                      T bit, Rest... args)
{
    return add_feature_mask_u32(mask | ((int(bit) >= 0 && int(bit) / 32 == (int)u32idx) ?
                                        (1 << (int(bit) % 32)) : 0),
                                u32idx, args...);
}

template<typename... Args>
static inline constexpr uint32_t get_feature_mask_u32(uint32_t u32idx, Args... args)
{
    return add_feature_mask_u32(uint32_t(0), u32idx, args...);
}

template<uint32_t... Is> struct seq{};
template<uint32_t N, uint32_t... Is>
struct gen_seq : gen_seq<N-1, N-1, Is...>{};
template<uint32_t... Is>
struct gen_seq<0, Is...> : seq<Is...>{};

template<size_t n, uint32_t... I, typename... Args>
static inline constexpr FeatureList<n>
_get_feature_mask(seq<I...>, Args... args)
{
    return FeatureList<n>{{get_feature_mask_u32(I, args...)...}};
}

template<size_t n, typename... Args>
static inline constexpr FeatureList<n> get_feature_masks(Args... args)
{
    return _get_feature_mask<n>(gen_seq<n>(), args...);
}

template<size_t n, uint32_t... I>
static inline constexpr FeatureList<n>
_feature_mask_or(seq<I...>, const FeatureList<n> &a, const FeatureList<n> &b)
{
    return FeatureList<n>{{(a[I] | b[I])...}};
}

template<size_t n>
static inline constexpr FeatureList<n> operator|(const FeatureList<n> &a, const FeatureList<n> &b)
{
    return _feature_mask_or<n>(gen_seq<n>(), a, b);
}

template<size_t n, uint32_t... I>
static inline constexpr FeatureList<n>
_feature_mask_and(seq<I...>, const FeatureList<n> &a, const FeatureList<n> &b)
{
    return FeatureList<n>{{(a[I] & b[I])...}};
}

template<size_t n>
static inline constexpr FeatureList<n> operator&(const FeatureList<n> &a, const FeatureList<n> &b)
{
    return _feature_mask_and<n>(gen_seq<n>(), a, b);
}

template<size_t n, uint32_t... I>
static inline constexpr FeatureList<n>
_feature_mask_not(seq<I...>, const FeatureList<n> &a)
{
    return FeatureList<n>{{(~a[I])...}};
}

template<size_t n>
static inline constexpr FeatureList<n> operator~(const FeatureList<n> &a)
{
    return _feature_mask_not<n>(gen_seq<n>(), a);
}

template<size_t n>
static inline void mask_features(const FeatureList<n> masks, uint32_t *features)
{
    for (size_t i = 0; i < n; i++) {
        features[i] = features[i] & masks[i];
    }
}

static inline std::string join_feature_strs(const std::vector<std::string> &strs)
{
    size_t nstr = strs.size();
    if (!nstr)
        return std::string("");
    std::string str = strs[0];
    for (size_t i = 1; i < nstr; i++)
        str += ',' + strs[i];
    return str;
}

static inline void append_ext_features(std::string &features, const std::string &ext_features)
{
    if (ext_features.empty())
        return;
    if (!features.empty())
        features.push_back(',');
    features.append(ext_features);
}

static inline void append_ext_features(std::vector<std::string> &features,
                                       const std::string &ext_features)
{
    if (ext_features.empty())
        return;
    const char *start = ext_features.c_str();
    for (const char *p = start; *p; p++) {
        if (*p == ',' || *p == '\0') {
            features.emplace_back(start, p - start);
            start = p + 1;
        }
    }
}

/**
 * Target specific type/constant definitions, always enable.
 */

struct FeatureName {
    const char *name;
    uint32_t bit; // bit index into a `uint32_t` array;
    uint32_t llvmver; // 0 if it is available on the oldest LLVM version we support
};

template<typename CPU, size_t n>
struct CPUSpec {
    const char *name;
    CPU cpu;
    CPU fallback;
    uint32_t llvmver;
    FeatureList<n> features;
};

struct FeatureDep {
    uint32_t feature;
    uint32_t dep;
};

template<size_t n>
static inline void enable_depends(FeatureList<n> &features, const FeatureDep *deps, size_t ndeps)
{
    bool changed = true;
    while (changed) {
        changed = false;
        for (ssize_t i = ndeps - 1; i >= 0; i--) {
            auto &dep = deps[i];
            if (!test_nbit(features, dep.feature) || test_nbit(features, dep.dep))
                continue;
            set_bit(features, dep.dep, true);
            changed = true;
        }
    }
}

template<size_t n>
static inline void disable_depends(FeatureList<n> &features, const FeatureDep *deps, size_t ndeps)
{
    bool changed = true;
    while (changed) {
        changed = false;
        for (ssize_t i = ndeps - 1; i >= 0; i--) {
            auto &dep = deps[i];
            if (!test_nbit(features, dep.feature) || test_nbit(features, dep.dep))
                continue;
            unset_bits(features, dep.feature);
            changed = true;
        }
    }
}

template<typename CPU, size_t n>
static const CPUSpec<CPU,n> *find_cpu(uint32_t cpu, const CPUSpec<CPU,n> *cpus, uint32_t ncpus)
{
    for (uint32_t i = 0; i < ncpus; i++) {
        if (cpu == uint32_t(cpus[i].cpu)) {
            return &cpus[i];
        }
    }
    return nullptr;
}

template<typename CPU, size_t n>
static const CPUSpec<CPU,n> *find_cpu(const char *name, const CPUSpec<CPU,n> *cpus, uint32_t ncpus)
{
    for (uint32_t i = 0; i < ncpus; i++) {
        if (strcmp(name, cpus[i].name) == 0) {
            return &cpus[i];
        }
    }
    return nullptr;
}

template<typename CPU, size_t n>
static const char *find_cpu_name(uint32_t cpu, const CPUSpec<CPU,n> *cpus, uint32_t ncpus)
{
    if (auto *spec = find_cpu(cpu, cpus, ncpus))
        return spec->name;
    return "generic";
}

template<typename CPU, size_t n>
static CPU find_cpu_id(const char *name, const CPUSpec<CPU,n> *cpus, uint32_t ncpus,
                       uint32_t def=0)
{
    if (auto *spec = find_cpu(name, cpus, ncpus))
        return spec->cpu;
    return static_cast<CPU>(def);
}

JL_UNUSED static uint32_t find_feature_bit(const FeatureName *features, size_t nfeatures,
                                           const char *str, size_t len)
{
    for (size_t i = 0; i < nfeatures; i++) {
        auto &feature = features[i];
        if (strncmp(feature.name, str, len) == 0 && feature.name[len] == 0) {
            return feature.bit;
        }
    }
    return (uint32_t)-1;
}

static inline std::vector<uint8_t> serialize_target_data(const char *name,
                                                         uint32_t nfeature,
                                                         const uint32_t *features_en,
                                                         const uint32_t *features_dis,
                                                         const char *ext_features)
{
    std::vector<uint8_t> res;
    auto add_data = [&] (const void *data, size_t sz) {
        size_t old_sz = res.size();
        res.resize(old_sz + sz);
        memcpy(&res[old_sz], data, sz);
    };
    add_data(&nfeature, 4);
    add_data(features_en, 4 * nfeature);
    add_data(features_dis, 4 * nfeature);
    uint32_t namelen = strlen(name);
    add_data(&namelen, 4);
    add_data(name, namelen);
    uint32_t ext_features_len = strlen(ext_features);
    add_data(&ext_features_len, 4);
    add_data(ext_features, ext_features_len);
    return res;
}

template<size_t n>
static inline std::vector<uint8_t> serialize_target_data(const char *name,
                                                         const FeatureList<n> &features_en,
                                                         const FeatureList<n> &features_dis,
                                                         const char *ext_features)
{
    return serialize_target_data(name, n, &features_en[0], &features_dis[0], ext_features);
}

template<size_t n>
struct TargetData {
    std::string name;
    std::string ext_features;
    struct {
        FeatureList<n> features;
        uint32_t flags;
    } en, dis;
    int base;
};

template<size_t n>
static inline std::vector<TargetData<n>> deserialize_target_data(const uint8_t *data)
{
    auto load_data = [&] (void *dest, size_t sz) {
        memcpy(dest, data, sz);
        data += sz;
    };
    auto load_string = [&] () {
        uint32_t len;
        load_data(&len, 4);
        std::string res((const char*)data, len);
        data += len;
        return res;
    };
    uint32_t ntarget;
    load_data(&ntarget, 4);
    std::vector<TargetData<n>> res(ntarget);
    for (uint32_t i = 0; i < ntarget; i++) {
        auto &target = res[i];
        load_data(&target.en.flags, 4);
        target.dis.flags = 0;
        // Starting serialized target data
        uint32_t nfeature;
        load_data(&nfeature, 4);
        assert(nfeature == n);
        load_data(&target.en.features[0], 4 * n);
        load_data(&target.dis.features[0], 4 * n);
        target.name = load_string();
        target.ext_features = load_string();
        target.base = 0;
    }
    return res;
}

// Try getting clone base argument. Return 1-based index. Return 0 if match failed.
static inline int get_clone_base(const char *start, const char *end)
{
    const char *prefix = "base(";
    const int prefix_len = strlen(prefix);
    if (end - start <= prefix_len)
        return 0;
    if (memcmp(start, prefix, prefix_len) != 0)
        return 0;
    start += prefix_len;
    if (*start > '9' || *start < '0')
        return 0;
    char *digit_end;
    auto idx = strtol(start, &digit_end, 10);
    if (idx < 0)
        return 0;
    if (*digit_end != ')' || digit_end + 1 != end)
        return 0;
    return (int)idx + 1;
}

template<size_t n, typename F>
static inline std::vector<TargetData<n>>
parse_cmdline(const char *option, F &&feature_cb)
{
    std::vector<TargetData<n>> res;
    if (!option)
        return res;
    TargetData<n> arg{};
    auto reset_arg = [&] {
        res.push_back(arg);
        arg.name.clear();
        arg.ext_features.clear();
        memset(&arg.en.features[0], 0, 4 * n);
        memset(&arg.dis.features[0], 0, 4 * n);
        arg.en.flags = 0;
        arg.dis.flags = 0;
    };
    const char *start = option;
    for (const char *p = option; ; p++) {
        switch (*p) {
        case ',':
        case ';':
        case '\0': {
            bool done = *p == '\0';
            bool next_target = *p == ';' || done;
            if (arg.name.empty()) {
                if (p == start)
                    jl_error("Invalid target option: empty CPU name");
                arg.name.append(start, p - start);
                start = p + 1;
                if (next_target)
                    reset_arg();
                if (done)
                    return res;
                continue;
            }
            bool disable = false;
            const char *full = start;
            const char *fname = full;
            start = p + 1;
            if (*full == '-') {
                disable = true;
                fname++;
            }
            else if (*full == '+') {
                fname++;
            }
            const char *clone_all = "clone_all";
            ssize_t clone_all_len = strlen(clone_all);
            if (p - fname == clone_all_len && memcmp(clone_all, fname, clone_all_len) == 0) {
                if (!disable) {
                    arg.en.flags |= JL_TARGET_CLONE_ALL;
                    arg.dis.flags &= ~JL_TARGET_CLONE_ALL;
                }
                else {
                    arg.dis.flags |= JL_TARGET_CLONE_ALL;
                    arg.en.flags &= ~JL_TARGET_CLONE_ALL;
                }
            }
            else if (int base = get_clone_base(fname, p)) {
                if (disable)
                    jl_error("Invalid target option: disabled base index.");
                base -= 1;
                if (base >= (int)res.size())
                    jl_error("Invalid target option: base index must refer to a previous target.");
                if (res[base].dis.flags & JL_TARGET_CLONE_ALL ||
                    !(res[base].en.flags & JL_TARGET_CLONE_ALL))
                    jl_error("Invalid target option: base target must be clone_all.");
                arg.base = base;
            }
            else {
                FeatureList<n> &list = disable ? arg.dis.features : arg.en.features;
                if (!feature_cb(fname, p - fname, list)) {
                    if (!arg.ext_features.empty())
                        arg.ext_features += ',';
                    arg.ext_features += disable ? '-' : '+';
                    arg.ext_features.append(fname, p - fname);
                }
            }
            if (next_target)
                reset_arg();
            if (done) {
                return res;
            }
        }
            JL_FALLTHROUGH;
        default:
            continue;
        }
    }
}

template<size_t n, typename F>
static inline std::vector<TargetData<n>> &get_cmdline_targets(F &&feature_cb)
{
    static std::vector<TargetData<n>> targets =
        parse_cmdline<n>(jl_options.cpu_target, std::forward<F>(feature_cb));
    return targets;
}

template<typename F>
static inline jl_sysimg_fptrs_t parse_sysimg(void *hdl, F &&callback)
{
    jl_sysimg_fptrs_t res = {nullptr, 0, nullptr, 0, nullptr, nullptr};
    // .data base
    auto data_base = (char*)jl_dlsym(hdl, "jl_sysimg_gvars_base");
    // .text base
    res.base = (const char*)jl_dlsym(hdl, "jl_sysimg_fvars_base");
    auto offsets = ((const int32_t*)jl_dlsym(hdl, "jl_sysimg_fvars_offsets")) + 1;
    uint32_t nfunc = ((const uint32_t*)offsets)[-1];
    res.offsets = offsets;

    void *ids = jl_dlsym(hdl, "jl_dispatch_target_ids");
    uint32_t target_idx = callback(ids);

    auto reloc_slots = ((const int32_t*)jl_dlsym(hdl, "jl_dispatch_reloc_slots")) + 1;
    auto nreloc = ((const uint32_t*)reloc_slots)[-1];
    auto clone_idxs = (const uint32_t*)jl_dlsym(hdl, "jl_dispatch_fvars_idxs");
    auto clone_offsets = (const int32_t*)jl_dlsym(hdl, "jl_dispatch_fvars_offsets");
    uint32_t tag_len = clone_idxs[0];
    clone_idxs += 1;
    const uint32_t tag_mask = 0x80000000u;
    const uint32_t val_mask = ~tag_mask;
    assert(tag_len & tag_mask);
    std::vector<const int32_t*> base_offsets = {res.offsets};
    // Find target
    for (uint32_t i = 0;i < target_idx;i++) {
        uint32_t len = val_mask & tag_len;
        if (tag_mask & tag_len) {
            if (i != 0)
                clone_offsets += nfunc;
            clone_idxs += len + 1;
        }
        else {
            clone_offsets += len;
            clone_idxs += len + 2;
        }
        tag_len = clone_idxs[-1];
        base_offsets.push_back(tag_len & tag_mask ? clone_offsets : nullptr);
    }

    bool clone_all = (tag_len & tag_mask) != 0;
    // Fill in return value
    if (clone_all) {
        // clone_all
        if (target_idx != 0) {
            res.offsets = clone_offsets;
        }
    }
    else {
        uint32_t base_idx = clone_idxs[0];
        assert(base_idx < target_idx);
        if (target_idx != 0) {
            res.offsets = base_offsets[base_idx];
            assert(res.offsets);
        }
        clone_idxs++;
        res.nclones = tag_len;
        res.clone_offsets = clone_offsets;
        res.clone_idxs = clone_idxs;
    }
    // Do relocation
    uint32_t reloc_i = 0;
    uint32_t len = val_mask & tag_len;
    for (uint32_t i = 0; i < len; i++) {
        uint32_t idx = clone_idxs[i];
        int32_t offset;
        if (clone_all) {
            offset = res.offsets[idx];
        }
        else if (idx & tag_mask) {
            idx = idx & val_mask;
            offset = clone_offsets[i];
        }
        else {
            continue;
        }
        bool found = false;
        for (; reloc_i < nreloc; reloc_i++) {
            auto reloc_idx = ((const uint32_t*)reloc_slots)[reloc_i * 2];
            if (reloc_idx == idx) {
                found = true;
                auto slot = (const void**)(data_base + reloc_slots[reloc_i * 2 + 1]);
                *slot = offset + res.base;
            }
            else if (reloc_idx > idx) {
                break;
            }
        }
        assert(found && "Cannot find GOT entry for cloned function.");
        (void)found;
    }

    return res;
}


// Debug helper

template<typename CPU, size_t n>
static inline void dump_cpu_spec(uint32_t cpu, const FeatureList<n> &features,
                                 const FeatureName *feature_names, uint32_t nfeature_names,
                                 const CPUSpec<CPU,n> *cpus, uint32_t ncpus)
{
    bool cpu_found = false;
    for (uint32_t i = 0;i < ncpus;i++) {
        if (cpu == uint32_t(cpus[i].cpu)) {
            cpu_found = true;
            jl_safe_printf("CPU: %s\n", cpus[i].name);
            break;
        }
    }
    if (!cpu_found)
        jl_safe_printf("CPU: generic\n");
    jl_safe_printf("Features:");
    bool first = true;
    for (uint32_t i = 0;i < nfeature_names;i++) {
        if (test_nbit(&features[0], feature_names[i].bit)) {
            if (first) {
                jl_safe_printf(" %s", feature_names[i].name);
                first = false;
            }
            else {
                jl_safe_printf(", %s", feature_names[i].name);
            }
        }
    }
    jl_safe_printf("\n");
}

}

#if defined(_CPU_X86_) || defined(_CPU_X86_64_)

#include "processor_x86.cpp"

#elif defined(_CPU_AARCH64_) || defined(_CPU_ARM_)

#include "processor_arm.cpp"

#else

#include "processor_fallback.cpp"

#endif