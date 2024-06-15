// Header File 
#import <Foundation/Foundation.h>             

// interface declaration 
@interface Person : NSObject
{ 
   NSString* _name; 
   int _age; 
} 

// Declaration of Parameterized constructor 
-(id)initWithName : (NSString*)name withAge : (int)age; 
-(void)display; 
@end

// Interface implementation 
@implementation Person 

// Definition of parameterized constructor 
-(id)initWithName : (NSString*)name withAge : (int)age 
{ 
   _name = name;
   _age = age; 
   return self; 
} 

// Display function to print the values 
-(void)display 
{ 
   NSLog(@"Person is %@ with age %d", 
         _name, _age);
}
@end

int main (int argc, const char * argv[]) 
{ 
NSAutoreleasePool * pool = [[NSAutoreleasePool alloc] init]; 
   
NSLog(@"Printing Persons using parameterized constructor"); 
   
// Object creation and initializing 
// the constructor during object creation 
Person *p = [[Person alloc] initWithName: @"Sam" withAge: 21];  
   
// calling display function to print 
// values with the help of object 
[p display]; 
[pool drain]; 
return 0; 
} 
