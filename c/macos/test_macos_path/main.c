
#include <CoreFoundation/CoreFoundation.h>
#include <stdio.h>


int main()
{
    char cpath[1024];
    // const char* pathin = "Macintosh
    // HD:/Users/me/Downloads/projects/chuck-max/patchers/examples/stk/mandolin.ck";
    const char* pathin = "/Users/me/Downloads/projects/chuck-max/patchers/"
                         "examples/stk/mandolin.ck";
    CFStringRef str_ref = CFStringCreateWithCString(NULL, pathin,
                                                    kCFStringEncodingUTF8);
    CFURLRef url = CFURLCreateWithString(NULL, str_ref, NULL);
    CFStringRef fixed_str = CFURLCopyFileSystemPath(url, kCFURLPOSIXPathStyle);
    CFStringGetCString(fixed_str, cpath, 1024, kCFStringEncodingUTF8);

    // const char * newpath = CFStringGetCStringPtr(fixed_str,
    // kCFStringEncodingUTF8);
    if (str_ref)
        CFRelease(str_ref);
    if (url)
        CFRelease(url);
    if (fixed_str)
        CFRelease(fixed_str);
    printf("text: %s\n", cpath);
}
