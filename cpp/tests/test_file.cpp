#include <cstdio>

// exceptions
class file_error { } ;
class open_error : public file_error { } ;
class close_error : public file_error { } ;
class write_error : public file_error { } ;

class file
{
public:
    file( const char* filename )
        :
        m_file_handle(std::fopen(filename, "w+"))
    {
        if( m_file_handle == NULL )
        {
            throw open_error() ;
        }
    }

    ~file()
    {
        std::fclose(m_file_handle) ;
    }

    void write( const char* str )
    {
        if( std::fputs(str, m_file_handle) == EOF )
        {
            throw write_error() ;
        }
    }

    void write( const char* buffer, std::size_t num_chars )
    {
        if( num_chars != 0
                &&
                std::fwrite(buffer, num_chars, 1, m_file_handle) == 0 )
        {
            throw write_error() ;
        }
    }

private:
    std::FILE* m_file_handle ;

    // copy and assignment not implemented; prevent their use by
    // declaring private.
    file( const file & ) ;
    file & operator=( const file & ) ;
} ;

int main()
{
    // open file (acquire resource)
    file logfile("logfile.txt") ;

    logfile.write("hello logfile!") ;
    // continue writing to logfile.txt ...

    // logfile.txt will automatically be closed because logfile's
    // destructor is always called when example_with_RAII() returns or
    // throws an exception.
    return 0;
}

