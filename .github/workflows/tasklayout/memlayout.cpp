/**
 * linux64: g++ -I $JULIA_HOME/include/julia/ memlayout.cpp -o memlayout
 * linux32: apt-get install g++-multilib
 *          g++ -march=pentium4 -m32 -I $JULIA_HOME/include/julia/ memlayout.cpp -o memlayout
 * macOS:   g++ -std=c++11 -I $JULIA_APP/Contents/Resources/julia/include/julia/ memlayout.cpp -o memlayout
 * win64:   g++ -I $JULIA_HOME/include/julia/ memlayout.cpp -o memlayout
 * win32:   g++ -I $JULIA_HOME/include/julia/ memlayout.cpp -o memlayout
 **/

#include <string>
#include <map>
#include <fstream>
#include <sstream>

#include <cstddef>

#include "julia.h"

std::string jl_ver(std::string sep=".") {
    std::ostringstream oss;
    oss << JULIA_VERSION_MAJOR << sep
        << JULIA_VERSION_MINOR << sep
        << JULIA_VERSION_PATCH;
    return oss.str();
}

std::string platform() {
#ifdef _WIN32
#ifdef _WIN64
    return "windows-x86_64";
#endif
    return "windows-x86";
#endif
#ifdef __APPLE__
    return "darwin-x86_64";
#endif
#ifdef __linux__
#ifdef __x86_64
    return "linux-x86_64";
#else
    return "linux-x86";
#endif
#endif
}

std::map<std::string, size_t> task_field_offsets() {
    std::map<std::string, size_t> data;

#define field_info(f) data[#f] = offsetof(jl_task_t, f)

    field_info(next);         // 131 142 153 154 160 170
    field_info(queue);        // 131 142 153 154 160 170
    field_info(tls);          // 131 142 153 154 160 170
    field_info(donenotify);   // 131 142 153 154 160 170
    field_info(result);       // 131 142 153 154 160 170

#if JULIA_VERSION_MAJOR == 1 && JULIA_VERSION_MINOR < 6
    field_info(state);        // 131 142 153 154
    field_info(exception);    // 131 142 153 154
    field_info(backtrace);    // 131 142 153 154
#endif

    field_info(logstate);     // 131 142 153 154 160 170
    field_info(start);        // 131 142 153 154 160 170
    field_info(sticky);       // 131 142 153 154 160 170

#if JULIA_VERSION_MAJOR == 1 && JULIA_VERSION_MINOR > 5
    field_info(_state);       //                 160 170
    field_info(_isexception); //                 160 170
#endif

#if JULIA_VERSION_MAJOR == 1 && JULIA_VERSION_MINOR > 6
    field_info(rngState0);    //                     170
    field_info(rngState1);    //                     170
    field_info(rngState2);    //                     170
    field_info(rngState3);    //                     170
#endif

    // hidden state
    field_info(gcstack);      // 131 142 153 154 160 170

#if JULIA_VERSION_MAJOR == 1 &&                                 \
    (JULIA_VERSION_MINOR > 6 || JULIA_VERSION_MINOR < 5 ||      \
        (JULIA_VERSION_MINOR == 5 && JULIA_VERSION_PATCH < 4))
    field_info(world_age);    // 131 142 153         170
#endif

#if JULIA_VERSION_MAJOR == 1 && JULIA_VERSION_MINOR > 6
    field_info(ptls);         //                     170
#endif
    field_info(tid);          // 131 142 153 154 160 170
    field_info(prio);         // 131 142 153 154 160 170
    field_info(excstack);     // 131 142 153 154 160 170
    field_info(eh);           // 131 142 153 154 160 170
    field_info(ctx);          // 131 142 153 154 160 170

#if JULIA_VERSION_MAJOR == 1 && JULIA_VERSION_MINOR < 6
    field_info(locks);        // 131 142 153 154
    field_info(timing_stack); // 131 142 153 154
#endif
    // field_info(copy_stack_ctx);

#if defined(JL_TSAN_ENABLED)
    field_info(tsan_state);   //                     170
#endif

    field_info(stkbuf);       // 131 142 153 154 160 170
    field_info(bufsz);        // 131 142 153 154 160 170
    // field_info(copy_stack);
    // field_info(started);

#undef field_info
    data["copy_stack"] = offsetof(jl_task_t, bufsz) + sizeof(size_t);
    data["sizeof_ctx"] = sizeof(((jl_task_t*)0)->ctx);
    // data["sizeof_stack_ctx"] = sizeof(((jl_task_t*)0)->copy_stack_ctx);
    data["tls_base_context"] = offsetof(struct _jl_tls_states_t, base_ctx);
    // data["tls_copy_stack_ctx"] = offsetof(struct _jl_tls_states_t, copy_stack_ctx);
    return data;
}

int main() {
    std::ofstream ofs(platform() +"-v" + jl_ver("_") + ".jl", std::ofstream::out);

    ofs << "ALL_TASK_OFFSETS[("
        << '"' << platform() << '"' << ", "
        << "v\"" << jl_ver() <<"\""
        <<")] = Dict(\n";
    ofs << "    :END => " << sizeof(jl_task_t) << ",\n";

    std::map<std::string, size_t> data = task_field_offsets();
    for(auto kv: data) {
        ofs << "    :" << kv.first << " => " << kv.second << ",\n";
    }
    ofs << ")\n";

    ofs.close();
    return 0;
}
