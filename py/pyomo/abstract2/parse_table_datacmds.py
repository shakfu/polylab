
# parse_table_datacmds.py
# This file is automatically generated. Do not edit.
_tabversion = '3.10'

_lr_method = 'LALR'

_lr_signature = 'COMMA LBRACE RBRACE SEMICOLON COLON COLONEQ LBRACKET RBRACKET LPAREN RPAREN WORD WORDWITHLBRACKET STRING BRACKETEDSTRING QUOTEDSTRING EQ TR ASTERISK INT_VAL FLOAT_VAL INCLUDE STORE TABLE END NAMESPACE DATA LOAD PARAM SETexpr : statements\n            | statements : statements statement\n                  | statement\n                  | statements NAMESPACE WORD LBRACE statements RBRACE\n                  | NAMESPACE WORD LBRACE statements RBRACE statement : SET WORD COLONEQ datastar SEMICOLON\n                 | SET WORDWITHLBRACKET args RBRACKET COLONEQ datastar SEMICOLON\n                 | SET WORD COLON itemstar COLONEQ datastar SEMICOLON\n                 | PARAM items COLONEQ datastar SEMICOLON\n                 | TABLE items COLONEQ datastar SEMICOLON\n                 | LOAD items SEMICOLON\n                 | STORE items SEMICOLON\n                 | INCLUDE WORD SEMICOLON\n                 | INCLUDE QUOTEDSTRING SEMICOLON\n                 | DATA SEMICOLON\n                 | END SEMICOLON\n    \n    datastar : data\n             |\n    \n    data : data WORD\n         | data STRING\n         | data QUOTEDSTRING\n         | data BRACKETEDSTRING\n         | data SET\n         | data TABLE\n         | data PARAM\n         | data INT_VAL\n         | data FLOAT_VAL\n         | data LPAREN\n         | data RPAREN\n         | data COMMA\n         | data ASTERISK\n         | WORD\n         | STRING\n         | QUOTEDSTRING\n         | BRACKETEDSTRING\n         | SET\n         | TABLE\n         | PARAM\n         | INT_VAL\n         | FLOAT_VAL\n         | LPAREN\n         | RPAREN\n         | COMMA\n         | ASTERISK\n    \n    args : arg\n         |\n    \n    arg : arg COMMA WORD\n         | arg COMMA STRING\n         | arg COMMA QUOTEDSTRING\n         | arg COMMA SET\n         | arg COMMA TABLE\n         | arg COMMA PARAM\n         | arg COMMA INT_VAL\n         | arg COMMA FLOAT_VAL\n         | WORD\n         | STRING\n         | QUOTEDSTRING\n         | SET\n         | TABLE\n         | PARAM\n         | INT_VAL\n         | FLOAT_VAL\n    \n    itemstar : items\n             |\n    \n    items : items WORD\n          | items STRING\n          | items QUOTEDSTRING\n          | items COMMA\n          | items COLON\n          | items LBRACE\n          | items RBRACE\n          | items LBRACKET\n          | items RBRACKET\n          | items TR\n          | items LPAREN\n          | items RPAREN\n          | items ASTERISK\n          | items EQ\n          | items SET\n          | items TABLE\n          | items PARAM\n          | items INT_VAL\n          | items FLOAT_VAL\n          | WORD\n          | STRING\n          | QUOTEDSTRING\n          | COMMA\n          | COLON\n          | LBRACKET\n          | RBRACKET\n          | LBRACE\n          | RBRACE\n          | TR\n          | LPAREN\n          | RPAREN\n          | ASTERISK\n          | EQ\n          | SET\n          | TABLE\n          | PARAM\n          | INT_VAL\n          | FLOAT_VAL\n    '
    
_lr_action_items = {'ASTERISK':([1,7,8,11,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,37,38,42,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,66,68,69,84,85,86,87,88,90,91,92,93,94,95,96,97,98,101,108,109,110,111,112,113,114,115,116,117,118,119,120,123,133,],[13,13,13,13,-97,-100,-91,-88,45,-103,-95,-85,-87,-101,-102,-99,-98,-96,-94,-92,-89,-86,-93,-90,45,45,45,-78,-81,-74,-69,-84,-76,-66,-68,-82,86,-83,-80,-79,-77,-75,-71,-70,-67,-72,-73,86,13,86,-43,-38,-45,-40,-42,-34,-44,-36,-39,-41,-33,110,-35,-37,45,-30,-25,-32,-27,-31,-21,-29,-23,-26,-28,-20,-22,-24,86,86,]),'TABLE':([0,1,2,6,7,8,11,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,37,38,40,42,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,66,67,68,69,80,81,82,83,84,85,86,87,88,90,91,92,93,94,95,96,97,98,99,101,104,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,133,134,135,138,139,],[1,14,-4,1,14,14,14,-97,-100,-91,-88,46,-103,-95,-85,-87,-101,-102,-99,-98,-96,-94,-92,-89,-86,-93,-90,-16,-17,-3,46,46,73,46,-78,-81,-74,-69,-84,-76,-66,-68,-82,85,-83,-80,-79,-77,-75,-71,-70,-67,-72,-73,85,-12,14,85,1,-13,-15,-14,-43,-38,-45,-40,-42,-34,-44,-36,-39,-41,-33,109,-35,-37,1,46,125,1,-11,-30,-25,-32,-27,-31,-21,-29,-23,-26,-28,-20,-22,-24,1,-10,85,-7,85,-6,-5,-9,-8,]),'RBRACKET':([1,7,8,11,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,37,38,40,42,45,46,47,48,49,50,51,52,53,55,56,57,58,59,60,61,62,63,64,68,70,71,72,73,74,75,76,77,78,79,101,125,126,127,128,129,130,131,132,],[15,15,15,15,-97,-100,-91,-88,47,-103,-95,-85,-87,-101,-102,-99,-98,-96,-94,-92,-89,-86,-93,-90,47,47,-47,47,-78,-81,-74,-69,-84,-76,-66,-68,-82,-83,-80,-79,-77,-75,-71,-70,-67,-72,-73,15,-62,-57,-46,-60,105,-63,-56,-58,-59,-61,47,-52,-51,-55,-48,-54,-49,-50,-53,]),'COMMA':([1,7,8,11,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,37,38,42,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,66,68,69,70,71,72,73,75,76,77,78,79,84,85,86,87,88,90,91,92,93,94,95,96,97,98,101,108,109,110,111,112,113,114,115,116,117,118,119,120,123,125,126,127,128,129,130,131,132,133,],[16,16,16,16,-97,-100,-91,-88,48,-103,-95,-85,-87,-101,-102,-99,-98,-96,-94,-92,-89,-86,-93,-90,48,48,48,-78,-81,-74,-69,-84,-76,-66,-68,-82,91,-83,-80,-79,-77,-75,-71,-70,-67,-72,-73,91,16,91,-62,-57,104,-60,-63,-56,-58,-59,-61,-43,-38,-45,-40,-42,-34,-44,-36,-39,-41,-33,112,-35,-37,48,-30,-25,-32,-27,-31,-21,-29,-23,-26,-28,-20,-22,-24,91,-52,-51,-55,-48,-54,-49,-50,-53,91,]),'BRACKETEDSTRING':([54,66,69,84,85,86,87,88,90,91,92,93,94,95,96,97,98,108,109,110,111,112,113,114,115,116,117,118,119,120,123,133,],[92,92,92,-43,-38,-45,-40,-42,-34,-44,-36,-39,-41,-33,115,-35,-37,-30,-25,-32,-27,-31,-21,-29,-23,-26,-28,-20,-22,-24,92,92,]),'LBRACKET':([1,7,8,11,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,37,38,42,45,46,47,48,49,50,51,52,53,55,56,57,58,59,60,61,62,63,64,68,101,],[32,32,32,32,-97,-100,-91,-88,64,-103,-95,-85,-87,-101,-102,-99,-98,-96,-94,-92,-89,-86,-93,-90,64,64,64,-78,-81,-74,-69,-84,-76,-66,-68,-82,-83,-80,-79,-77,-75,-71,-70,-67,-72,-73,32,64,]),'WORDWITHLBRACKET':([9,],[40,]),'SEMICOLON':([4,5,13,14,15,16,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,38,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,66,69,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,100,103,108,109,110,111,112,113,114,115,116,117,118,119,120,123,133,136,137,],[33,34,-97,-100,-91,-88,-103,-95,-85,-87,-101,-102,-99,-98,-96,-94,-92,-89,-86,-93,-90,67,81,82,83,-78,-81,-74,-69,-84,-76,-66,-68,-82,-19,-83,-80,-79,-77,-75,-71,-70,-67,-72,-73,-19,-19,-43,-38,-45,-40,-42,107,-34,-44,-36,-39,-41,-33,-18,-35,-37,122,124,-30,-25,-32,-27,-31,-21,-29,-23,-26,-28,-20,-22,-24,-19,-19,138,139,]),'END':([0,2,6,33,34,35,67,80,81,82,83,99,106,107,121,122,124,134,135,138,139,],[5,-4,5,-16,-17,-3,-12,5,-13,-15,-14,5,5,-11,5,-10,-7,-6,-5,-9,-8,]),'FLOAT_VAL':([1,7,8,11,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,37,38,40,42,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,66,68,69,84,85,86,87,88,90,91,92,93,94,95,96,97,98,101,104,108,109,110,111,112,113,114,115,116,117,118,119,120,123,133,],[18,18,18,18,-97,-100,-91,-88,49,-103,-95,-85,-87,-101,-102,-99,-98,-96,-94,-92,-89,-86,-93,-90,49,49,75,49,-78,-81,-74,-69,-84,-76,-66,-68,-82,94,-83,-80,-79,-77,-75,-71,-70,-67,-72,-73,94,18,94,-43,-38,-45,-40,-42,-34,-44,-36,-39,-41,-33,117,-35,-37,49,127,-30,-25,-32,-27,-31,-21,-29,-23,-26,-28,-20,-22,-24,94,94,]),'LPAREN':([1,7,8,11,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,37,38,42,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,66,68,69,84,85,86,87,88,90,91,92,93,94,95,96,97,98,101,108,109,110,111,112,113,114,115,116,117,118,119,120,123,133,],[19,19,19,19,-97,-100,-91,-88,50,-103,-95,-85,-87,-101,-102,-99,-98,-96,-94,-92,-89,-86,-93,-90,50,50,50,-78,-81,-74,-69,-84,-76,-66,-68,-82,88,-83,-80,-79,-77,-75,-71,-70,-67,-72,-73,88,19,88,-43,-38,-45,-40,-42,-34,-44,-36,-39,-41,-33,114,-35,-37,50,-30,-25,-32,-27,-31,-21,-29,-23,-26,-28,-20,-22,-24,88,88,]),'WORD':([1,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,36,37,38,40,42,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,66,68,69,84,85,86,87,88,90,91,92,93,94,95,96,97,98,101,104,108,109,110,111,112,113,114,115,116,117,118,119,120,123,133,],[20,20,20,39,41,20,44,-97,-100,-91,-88,51,-103,-95,-85,-87,-101,-102,-99,-98,-96,-94,-92,-89,-86,-93,-90,65,51,51,76,51,-78,-81,-74,-69,-84,-76,-66,-68,-82,95,-83,-80,-79,-77,-75,-71,-70,-67,-72,-73,95,20,95,-43,-38,-45,-40,-42,-34,-44,-36,-39,-41,-33,118,-35,-37,51,128,-30,-25,-32,-27,-31,-21,-29,-23,-26,-28,-20,-22,-24,95,95,]),'QUOTEDSTRING':([1,7,8,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,37,38,40,42,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,66,68,69,84,85,86,87,88,90,91,92,93,94,95,96,97,98,101,104,108,109,110,111,112,113,114,115,116,117,118,119,120,123,133,],[21,21,21,21,43,-97,-100,-91,-88,52,-103,-95,-85,-87,-101,-102,-99,-98,-96,-94,-92,-89,-86,-93,-90,52,52,77,52,-78,-81,-74,-69,-84,-76,-66,-68,-82,97,-83,-80,-79,-77,-75,-71,-70,-67,-72,-73,97,21,97,-43,-38,-45,-40,-42,-34,-44,-36,-39,-41,-33,119,-35,-37,52,131,-30,-25,-32,-27,-31,-21,-29,-23,-26,-28,-20,-22,-24,97,97,]),'PARAM':([0,1,2,6,7,8,11,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,37,38,40,42,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,66,67,68,69,80,81,82,83,84,85,86,87,88,90,91,92,93,94,95,96,97,98,99,101,104,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,133,134,135,138,139,],[7,22,-4,7,22,22,22,-97,-100,-91,-88,53,-103,-95,-85,-87,-101,-102,-99,-98,-96,-94,-92,-89,-86,-93,-90,-16,-17,-3,53,53,79,53,-78,-81,-74,-69,-84,-76,-66,-68,-82,93,-83,-80,-79,-77,-75,-71,-70,-67,-72,-73,93,-12,22,93,7,-13,-15,-14,-43,-38,-45,-40,-42,-34,-44,-36,-39,-41,-33,116,-35,-37,7,53,132,7,-11,-30,-25,-32,-27,-31,-21,-29,-23,-26,-28,-20,-22,-24,7,-10,93,-7,93,-6,-5,-9,-8,]),'LOAD':([0,2,6,33,34,35,67,80,81,82,83,99,106,107,121,122,124,134,135,138,139,],[8,-4,8,-16,-17,-3,-12,8,-13,-15,-14,8,8,-11,8,-10,-7,-6,-5,-9,-8,]),'COLONEQ':([13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,37,39,45,46,47,48,49,50,51,52,53,55,56,57,58,59,60,61,62,63,64,68,101,102,105,],[-97,-100,-91,-88,54,-103,-95,-85,-87,-101,-102,-99,-98,-96,-94,-92,-89,-86,-93,-90,66,69,-78,-81,-74,-69,-84,-76,-66,-68,-82,-83,-80,-79,-77,-75,-71,-70,-67,-72,-73,-65,-64,123,133,]),'INT_VAL':([1,7,8,11,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,37,38,40,42,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,66,68,69,84,85,86,87,88,90,91,92,93,94,95,96,97,98,101,104,108,109,110,111,112,113,114,115,116,117,118,119,120,123,133,],[23,23,23,23,-97,-100,-91,-88,55,-103,-95,-85,-87,-101,-102,-99,-98,-96,-94,-92,-89,-86,-93,-90,55,55,70,55,-78,-81,-74,-69,-84,-76,-66,-68,-82,87,-83,-80,-79,-77,-75,-71,-70,-67,-72,-73,87,23,87,-43,-38,-45,-40,-42,-34,-44,-36,-39,-41,-33,111,-35,-37,55,129,-30,-25,-32,-27,-31,-21,-29,-23,-26,-28,-20,-22,-24,87,87,]),'SET':([0,1,2,6,7,8,11,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,37,38,40,42,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,66,67,68,69,80,81,82,83,84,85,86,87,88,90,91,92,93,94,95,96,97,98,99,101,104,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,133,134,135,138,139,],[9,24,-4,9,24,24,24,-97,-100,-91,-88,56,-103,-95,-85,-87,-101,-102,-99,-98,-96,-94,-92,-89,-86,-93,-90,-16,-17,-3,56,56,78,56,-78,-81,-74,-69,-84,-76,-66,-68,-82,98,-83,-80,-79,-77,-75,-71,-70,-67,-72,-73,98,-12,24,98,9,-13,-15,-14,-43,-38,-45,-40,-42,-34,-44,-36,-39,-41,-33,120,-35,-37,9,56,126,9,-11,-30,-25,-32,-27,-31,-21,-29,-23,-26,-28,-20,-22,-24,9,-10,98,-7,98,-6,-5,-9,-8,]),'EQ':([1,7,8,11,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,37,38,42,45,46,47,48,49,50,51,52,53,55,56,57,58,59,60,61,62,63,64,68,101,],[25,25,25,25,-97,-100,-91,-88,57,-103,-95,-85,-87,-101,-102,-99,-98,-96,-94,-92,-89,-86,-93,-90,57,57,57,-78,-81,-74,-69,-84,-76,-66,-68,-82,-83,-80,-79,-77,-75,-71,-70,-67,-72,-73,25,57,]),'RPAREN':([1,7,8,11,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,37,38,42,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,66,68,69,84,85,86,87,88,90,91,92,93,94,95,96,97,98,101,108,109,110,111,112,113,114,115,116,117,118,119,120,123,133,],[26,26,26,26,-97,-100,-91,-88,58,-103,-95,-85,-87,-101,-102,-99,-98,-96,-94,-92,-89,-86,-93,-90,58,58,58,-78,-81,-74,-69,-84,-76,-66,-68,-82,84,-83,-80,-79,-77,-75,-71,-70,-67,-72,-73,84,26,84,-43,-38,-45,-40,-42,-34,-44,-36,-39,-41,-33,108,-35,-37,58,-30,-25,-32,-27,-31,-21,-29,-23,-26,-28,-20,-22,-24,84,84,]),'TR':([1,7,8,11,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,37,38,42,45,46,47,48,49,50,51,52,53,55,56,57,58,59,60,61,62,63,64,68,101,],[27,27,27,27,-97,-100,-91,-88,59,-103,-95,-85,-87,-101,-102,-99,-98,-96,-94,-92,-89,-86,-93,-90,59,59,59,-78,-81,-74,-69,-84,-76,-66,-68,-82,-83,-80,-79,-77,-75,-71,-70,-67,-72,-73,27,59,]),'STRING':([1,7,8,11,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,37,38,40,42,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,66,68,69,84,85,86,87,88,90,91,92,93,94,95,96,97,98,101,104,108,109,110,111,112,113,114,115,116,117,118,119,120,123,133,],[30,30,30,30,-97,-100,-91,-88,62,-103,-95,-85,-87,-101,-102,-99,-98,-96,-94,-92,-89,-86,-93,-90,62,62,71,62,-78,-81,-74,-69,-84,-76,-66,-68,-82,90,-83,-80,-79,-77,-75,-71,-70,-67,-72,-73,90,30,90,-43,-38,-45,-40,-42,-34,-44,-36,-39,-41,-33,113,-35,-37,62,130,-30,-25,-32,-27,-31,-21,-29,-23,-26,-28,-20,-22,-24,90,90,]),'NAMESPACE':([0,2,6,33,34,35,67,80,81,82,83,99,106,107,121,122,124,134,135,138,139,],[10,-4,36,-16,-17,-3,-12,10,-13,-15,-14,10,36,-11,36,-10,-7,-6,-5,-9,-8,]),'STORE':([0,2,6,33,34,35,67,80,81,82,83,99,106,107,121,122,124,134,135,138,139,],[11,-4,11,-16,-17,-3,-12,11,-13,-15,-14,11,11,-11,11,-10,-7,-6,-5,-9,-8,]),'LBRACE':([1,7,8,11,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,37,38,41,42,45,46,47,48,49,50,51,52,53,55,56,57,58,59,60,61,62,63,64,65,68,101,],[28,28,28,28,-97,-100,-91,-88,60,-103,-95,-85,-87,-101,-102,-99,-98,-96,-94,-92,-89,-86,-93,-90,60,60,80,60,-78,-81,-74,-69,-84,-76,-66,-68,-82,-83,-80,-79,-77,-75,-71,-70,-67,-72,-73,99,28,60,]),'COLON':([1,7,8,11,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,37,38,39,42,45,46,47,48,49,50,51,52,53,55,56,57,58,59,60,61,62,63,64,68,101,],[29,29,29,29,-97,-100,-91,-88,61,-103,-95,-85,-87,-101,-102,-99,-98,-96,-94,-92,-89,-86,-93,-90,61,61,68,61,-78,-81,-74,-69,-84,-76,-66,-68,-82,-83,-80,-79,-77,-75,-71,-70,-67,-72,-73,29,61,]),'DATA':([0,2,6,33,34,35,67,80,81,82,83,99,106,107,121,122,124,134,135,138,139,],[4,-4,4,-16,-17,-3,-12,4,-13,-15,-14,4,4,-11,4,-10,-7,-6,-5,-9,-8,]),'RBRACE':([1,2,7,8,11,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,37,38,42,45,46,47,48,49,50,51,52,53,55,56,57,58,59,60,61,62,63,64,67,68,81,82,83,101,106,107,121,122,124,134,135,138,139,],[31,-4,31,31,31,-97,-100,-91,-88,63,-103,-95,-85,-87,-101,-102,-99,-98,-96,-94,-92,-89,-86,-93,-90,-16,-17,-3,63,63,63,-78,-81,-74,-69,-84,-76,-66,-68,-82,-83,-80,-79,-77,-75,-71,-70,-67,-72,-73,-12,31,-13,-15,-14,63,134,-11,135,-10,-7,-6,-5,-9,-8,]),'$end':([0,2,3,6,33,34,35,67,81,82,83,107,122,124,134,135,138,139,],[-2,-4,0,-1,-16,-17,-3,-12,-13,-15,-14,-11,-10,-7,-6,-5,-9,-8,]),'INCLUDE':([0,2,6,33,34,35,67,80,81,82,83,99,106,107,121,122,124,134,135,138,139,],[12,-4,12,-16,-17,-3,-12,12,-13,-15,-14,12,12,-11,12,-10,-7,-6,-5,-9,-8,]),}

_lr_action = {}
for _k, _v in _lr_action_items.items():
   for _x,_y in zip(_v[0],_v[1]):
      if not _x in _lr_action:  _lr_action[_x] = {}
      _lr_action[_x][_k] = _y
del _lr_action_items

_lr_goto_items = {'datastar':([54,66,69,123,133,],[89,100,103,136,137,]),'statement':([0,6,80,99,106,121,],[2,35,2,2,35,35,]),'expr':([0,],[3,]),'itemstar':([68,],[102,]),'items':([1,7,8,11,68,],[17,37,38,42,101,]),'data':([54,66,69,123,133,],[96,96,96,96,96,]),'statements':([0,80,99,],[6,106,121,]),'args':([40,],[74,]),'arg':([40,],[72,]),}

_lr_goto = {}
for _k, _v in _lr_goto_items.items():
   for _x, _y in zip(_v[0], _v[1]):
       if not _x in _lr_goto: _lr_goto[_x] = {}
       _lr_goto[_x][_k] = _y
del _lr_goto_items
_lr_productions = [
  ("S' -> expr","S'",1,None,None,None),
  ('expr -> statements','expr',1,'p_expr','parse_datacmds.py',165),
  ('expr -> <empty>','expr',0,'p_expr','parse_datacmds.py',166),
  ('statements -> statements statement','statements',2,'p_statements','parse_datacmds.py',180),
  ('statements -> statement','statements',1,'p_statements','parse_datacmds.py',181),
  ('statements -> statements NAMESPACE WORD LBRACE statements RBRACE','statements',6,'p_statements','parse_datacmds.py',182),
  ('statements -> NAMESPACE WORD LBRACE statements RBRACE','statements',5,'p_statements','parse_datacmds.py',183),
  ('statement -> SET WORD COLONEQ datastar SEMICOLON','statement',5,'p_statement','parse_datacmds.py',205),
  ('statement -> SET WORDWITHLBRACKET args RBRACKET COLONEQ datastar SEMICOLON','statement',7,'p_statement','parse_datacmds.py',206),
  ('statement -> SET WORD COLON itemstar COLONEQ datastar SEMICOLON','statement',7,'p_statement','parse_datacmds.py',207),
  ('statement -> PARAM items COLONEQ datastar SEMICOLON','statement',5,'p_statement','parse_datacmds.py',208),
  ('statement -> TABLE items COLONEQ datastar SEMICOLON','statement',5,'p_statement','parse_datacmds.py',209),
  ('statement -> LOAD items SEMICOLON','statement',3,'p_statement','parse_datacmds.py',210),
  ('statement -> STORE items SEMICOLON','statement',3,'p_statement','parse_datacmds.py',211),
  ('statement -> INCLUDE WORD SEMICOLON','statement',3,'p_statement','parse_datacmds.py',212),
  ('statement -> INCLUDE QUOTEDSTRING SEMICOLON','statement',3,'p_statement','parse_datacmds.py',213),
  ('statement -> DATA SEMICOLON','statement',2,'p_statement','parse_datacmds.py',214),
  ('statement -> END SEMICOLON','statement',2,'p_statement','parse_datacmds.py',215),
  ('datastar -> data','datastar',1,'p_datastar','parse_datacmds.py',241),
  ('datastar -> <empty>','datastar',0,'p_datastar','parse_datacmds.py',242),
  ('data -> data WORD','data',2,'p_data','parse_datacmds.py',251),
  ('data -> data STRING','data',2,'p_data','parse_datacmds.py',252),
  ('data -> data QUOTEDSTRING','data',2,'p_data','parse_datacmds.py',253),
  ('data -> data BRACKETEDSTRING','data',2,'p_data','parse_datacmds.py',254),
  ('data -> data SET','data',2,'p_data','parse_datacmds.py',255),
  ('data -> data TABLE','data',2,'p_data','parse_datacmds.py',256),
  ('data -> data PARAM','data',2,'p_data','parse_datacmds.py',257),
  ('data -> data INT_VAL','data',2,'p_data','parse_datacmds.py',258),
  ('data -> data FLOAT_VAL','data',2,'p_data','parse_datacmds.py',259),
  ('data -> data LPAREN','data',2,'p_data','parse_datacmds.py',260),
  ('data -> data RPAREN','data',2,'p_data','parse_datacmds.py',261),
  ('data -> data COMMA','data',2,'p_data','parse_datacmds.py',262),
  ('data -> data ASTERISK','data',2,'p_data','parse_datacmds.py',263),
  ('data -> WORD','data',1,'p_data','parse_datacmds.py',264),
  ('data -> STRING','data',1,'p_data','parse_datacmds.py',265),
  ('data -> QUOTEDSTRING','data',1,'p_data','parse_datacmds.py',266),
  ('data -> BRACKETEDSTRING','data',1,'p_data','parse_datacmds.py',267),
  ('data -> SET','data',1,'p_data','parse_datacmds.py',268),
  ('data -> TABLE','data',1,'p_data','parse_datacmds.py',269),
  ('data -> PARAM','data',1,'p_data','parse_datacmds.py',270),
  ('data -> INT_VAL','data',1,'p_data','parse_datacmds.py',271),
  ('data -> FLOAT_VAL','data',1,'p_data','parse_datacmds.py',272),
  ('data -> LPAREN','data',1,'p_data','parse_datacmds.py',273),
  ('data -> RPAREN','data',1,'p_data','parse_datacmds.py',274),
  ('data -> COMMA','data',1,'p_data','parse_datacmds.py',275),
  ('data -> ASTERISK','data',1,'p_data','parse_datacmds.py',276),
  ('args -> arg','args',1,'p_args','parse_datacmds.py',299),
  ('args -> <empty>','args',0,'p_args','parse_datacmds.py',300),
  ('arg -> arg COMMA WORD','arg',3,'p_arg','parse_datacmds.py',309),
  ('arg -> arg COMMA STRING','arg',3,'p_arg','parse_datacmds.py',310),
  ('arg -> arg COMMA QUOTEDSTRING','arg',3,'p_arg','parse_datacmds.py',311),
  ('arg -> arg COMMA SET','arg',3,'p_arg','parse_datacmds.py',312),
  ('arg -> arg COMMA TABLE','arg',3,'p_arg','parse_datacmds.py',313),
  ('arg -> arg COMMA PARAM','arg',3,'p_arg','parse_datacmds.py',314),
  ('arg -> arg COMMA INT_VAL','arg',3,'p_arg','parse_datacmds.py',315),
  ('arg -> arg COMMA FLOAT_VAL','arg',3,'p_arg','parse_datacmds.py',316),
  ('arg -> WORD','arg',1,'p_arg','parse_datacmds.py',317),
  ('arg -> STRING','arg',1,'p_arg','parse_datacmds.py',318),
  ('arg -> QUOTEDSTRING','arg',1,'p_arg','parse_datacmds.py',319),
  ('arg -> SET','arg',1,'p_arg','parse_datacmds.py',320),
  ('arg -> TABLE','arg',1,'p_arg','parse_datacmds.py',321),
  ('arg -> PARAM','arg',1,'p_arg','parse_datacmds.py',322),
  ('arg -> INT_VAL','arg',1,'p_arg','parse_datacmds.py',323),
  ('arg -> FLOAT_VAL','arg',1,'p_arg','parse_datacmds.py',324),
  ('itemstar -> items','itemstar',1,'p_itemstar','parse_datacmds.py',347),
  ('itemstar -> <empty>','itemstar',0,'p_itemstar','parse_datacmds.py',348),
  ('items -> items WORD','items',2,'p_items','parse_datacmds.py',357),
  ('items -> items STRING','items',2,'p_items','parse_datacmds.py',358),
  ('items -> items QUOTEDSTRING','items',2,'p_items','parse_datacmds.py',359),
  ('items -> items COMMA','items',2,'p_items','parse_datacmds.py',360),
  ('items -> items COLON','items',2,'p_items','parse_datacmds.py',361),
  ('items -> items LBRACE','items',2,'p_items','parse_datacmds.py',362),
  ('items -> items RBRACE','items',2,'p_items','parse_datacmds.py',363),
  ('items -> items LBRACKET','items',2,'p_items','parse_datacmds.py',364),
  ('items -> items RBRACKET','items',2,'p_items','parse_datacmds.py',365),
  ('items -> items TR','items',2,'p_items','parse_datacmds.py',366),
  ('items -> items LPAREN','items',2,'p_items','parse_datacmds.py',367),
  ('items -> items RPAREN','items',2,'p_items','parse_datacmds.py',368),
  ('items -> items ASTERISK','items',2,'p_items','parse_datacmds.py',369),
  ('items -> items EQ','items',2,'p_items','parse_datacmds.py',370),
  ('items -> items SET','items',2,'p_items','parse_datacmds.py',371),
  ('items -> items TABLE','items',2,'p_items','parse_datacmds.py',372),
  ('items -> items PARAM','items',2,'p_items','parse_datacmds.py',373),
  ('items -> items INT_VAL','items',2,'p_items','parse_datacmds.py',374),
  ('items -> items FLOAT_VAL','items',2,'p_items','parse_datacmds.py',375),
  ('items -> WORD','items',1,'p_items','parse_datacmds.py',376),
  ('items -> STRING','items',1,'p_items','parse_datacmds.py',377),
  ('items -> QUOTEDSTRING','items',1,'p_items','parse_datacmds.py',378),
  ('items -> COMMA','items',1,'p_items','parse_datacmds.py',379),
  ('items -> COLON','items',1,'p_items','parse_datacmds.py',380),
  ('items -> LBRACKET','items',1,'p_items','parse_datacmds.py',381),
  ('items -> RBRACKET','items',1,'p_items','parse_datacmds.py',382),
  ('items -> LBRACE','items',1,'p_items','parse_datacmds.py',383),
  ('items -> RBRACE','items',1,'p_items','parse_datacmds.py',384),
  ('items -> TR','items',1,'p_items','parse_datacmds.py',385),
  ('items -> LPAREN','items',1,'p_items','parse_datacmds.py',386),
  ('items -> RPAREN','items',1,'p_items','parse_datacmds.py',387),
  ('items -> ASTERISK','items',1,'p_items','parse_datacmds.py',388),
  ('items -> EQ','items',1,'p_items','parse_datacmds.py',389),
  ('items -> SET','items',1,'p_items','parse_datacmds.py',390),
  ('items -> TABLE','items',1,'p_items','parse_datacmds.py',391),
  ('items -> PARAM','items',1,'p_items','parse_datacmds.py',392),
  ('items -> INT_VAL','items',1,'p_items','parse_datacmds.py',393),
  ('items -> FLOAT_VAL','items',1,'p_items','parse_datacmds.py',394),
]