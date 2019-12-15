#include "semantic.h"


AST *root;
int semanticErrors = 0;
int getSemanticErrors(){
    return semanticErrors;
}


int checkArithmeticDatatype(AST* node1, AST* node2){
    int datatype = DATATYPE_BYTE;

    if(node1->datatype == DATATYPE_BOOL || node2->datatype == DATATYPE_BOOL ||
        node1->datatype == DATATYPE_ERROR || node2->datatype == DATATYPE_ERROR)
        return DATATYPE_ERROR;

    if(node1->datatype == DATATYPE_INT || node2->datatype == DATATYPE_INT)
        datatype = DATATYPE_INT;

    if(node1->datatype == DATATYPE_LONG || node2->datatype == DATATYPE_LONG)
        datatype = DATATYPE_LONG;

    if(node1->datatype == DATATYPE_FLOAT || node2->datatype == DATATYPE_FLOAT)
        datatype = DATATYPE_FLOAT;

    return datatype;
}


int isNumericType(int datatype){
    return (datatype == DATATYPE_BYTE || datatype == DATATYPE_INT || datatype == DATATYPE_FLOAT || datatype == DATATYPE_LONG);
}

int isRelationalOP(int nodetype){
    return (nodetype == AST_LESSER|| nodetype == AST_GREATER || nodetype == AST_EQUAL 
            || nodetype == AST_LE || nodetype == AST_GE);
}

int isArithmeticOp(int nodetype){
    return (nodetype == AST_ADD || nodetype == AST_SUB || nodetype == AST_MUL || nodetype == AST_DIV);
}

int isLogicalOp(int nodetype){
    return (nodetype == AST_AND || nodetype == AST_OR || nodetype == AST_NOT);
}

int isIntType(int datatype){
    return (datatype == DATATYPE_BYTE || datatype == DATATYPE_INT || datatype == DATATYPE_LONG 
           || datatype == DATATYPE_CHAR || datatype == DATATYPE_FLOAT);
}

int areCompatible(int datatype1, int datatype2){
    return (isIntType(datatype1) && isIntType(datatype2)) || (datatype1 == datatype2);
}

int testVetElems(AST *node, int datatype){
    if (node){
        if (!areCompatible(node->son[0]->symbol->datatype, datatype))
            return 0;
        if (node->son[1] != NULL)
            return testVetElems(node->son[1], datatype);
    }
    return 1;
}

void isReturnCompatible(AST* node, int datatype){
    if(!node) return;
    if(node->type == AST_RET){
        if(!areCompatible(node->son[0]->datatype, datatype)){
            printf("Semantic ERROR in line %d. Return statement with wrong datatype.\n", node->line + 1);
            semanticErrors++;
        }
    }
    for(int i = 0; i < MAX_SONS; i++){
        isReturnCompatible(node->son[i], datatype);
    }
}


void checkReturns(AST* node){
    if(node != NULL && node->type == AST_FUNC){
        isReturnCompatible(node, node->symbol->datatype);
    }

    for(int i = 0; i < MAX_SONS; i++){
        if(node->son[i] != NULL)
            checkReturns(node->son[i]);
    }
}

void setDataTypes(AST *node){

    if(node->son[0]->type == AST_LONG)
        node->symbol->datatype = DATATYPE_LONG;

    if(node->son[0]->type == AST_INT)
        node->symbol->datatype = DATATYPE_INT;

    if(node->son[0]->type == AST_FLOAT)
        node->symbol->datatype = DATATYPE_FLOAT;

    if(node->son[0]->type == AST_BYTE)
        node->symbol->datatype = DATATYPE_BYTE;

    if(node->son[0]->type == AST_BOOL)
        node->symbol->datatype = DATATYPE_BOOL;
}



void checkAndSetTypes(AST *node){
    if(!node) return;

    switch(node->type){
        case AST_VARDEC:
            if(node->symbol->type != SYMBOL_IDENTIFIER){
                fprintf(stderr, "Semantic ERROR: Symbol %s already declared at line %d.\n", node->symbol->text, node->line + 1);
                semanticErrors++;
            } else{
                node->symbol->type = SYMBOL_SCALAR;
                setDataTypes(node);
                //node->datatype = node->symbol->datatype;  
            }              

            if (!areCompatible(node->symbol->datatype, node->son[1]->symbol->datatype)){
                fprintf(stderr, "Semantic ERROR: types are not compatible at line %d.\n",  node->line + 1);
                semanticErrors++;
            }
            break;

        case AST_VEC:
            if (node->symbol->type != SYMBOL_IDENTIFIER){
                fprintf(stderr, "Semantic ERROR: Vector %s already declared at line %d.\n", node->symbol->text, node->line + 1);
                semanticErrors++;
            }else{
                node->symbol->type = SYMBOL_VECTOR;
                setDataTypes(node);
            }

            if (!testVetElems(node->son[2], node->symbol->datatype)){
                 fprintf(stderr, "Semantic ERROR: Incompatible types in vector declaration at line %d\n", node->line + 1);
                semanticErrors++;
            }
            break;

        case AST_FUNC: 
            if (node->symbol->type != SYMBOL_IDENTIFIER){
                fprintf(stderr, "Semantic ERROR: Function %s already declared at line %d.\n", node->symbol->text, node->line + 1);
                semanticErrors++;
            } else {
                node->symbol->type = SYMBOL_FUNC;
                setDataTypes(node);
            }
            break;

        case AST_PARAM:
            if (node->symbol->type != SYMBOL_IDENTIFIER){
              fprintf(stderr, "Semantic ERROR: Parameter %s already declared at line %d.\n", node->symbol->text, node->line + 1);
              semanticErrors++;
            } else {
                node->symbol->type = SYMBOL_PARAM;
                setDataTypes(node);
            }
        default:
            break;        
    }

    for(int i = 0; i < MAX_SONS; i++) {
        checkAndSetTypes(node->son[i]);
    }
}



void setNodeTypes(AST *node){
    if (node == NULL)
        return;

    for (int i = 0; i < MAX_SONS; i++){
        setNodeTypes(node->son[i]);
    }

    if (node->type == AST_SYMBOL){
        if(node->symbol->type == SYMBOL_VECTOR || node->symbol->type == SYMBOL_FUNC){
            fprintf(stderr, "Semantic ERROR: %s incorrect use of scalar at line %d.\n", node->symbol->text, node->line + 1);
            semanticErrors++;
        }
        node->datatype = node->symbol->datatype;
    }

    else if (node->type == AST_FUNC || node->type == AST_VEC){
        node->datatype = node->symbol->datatype;
    }
    else if (node->type == AST_EXP){
        node->datatype = node->son[0]->datatype;
    }
    else if (isArithmeticOp(node->type)){
        AST *son0 = node->son[0];
        AST *son1 = node->son[1];
        if (!areCompatible(son0->datatype, son1->datatype) || son0->datatype == DATATYPE_BOOL || son1->datatype == DATATYPE_BOOL){
           fprintf(stderr, "Semantic ERROR: incompatible types for arithmetic operation at line %d.\n", node->line + 1);
           semanticErrors++;
        }

        if(son0->datatype > son1->datatype)
            node->datatype = son0->datatype;
        else
            node->datatype = son1->datatype;
        
    }
    else if (isRelationalOP(node->type)){
        if (!isNumericType(node->son[0]->datatype) || !isNumericType(node->son[1]->datatype)){
            fprintf(stderr, "Semantic ERROR: incompatible types for relational operation at line %d.\n", node->line + 1);
            semanticErrors++;
        }
        node->datatype = DATATYPE_BOOL;
    }
    else if (isLogicalOp(node->type)){
        if (node->type == AST_NOT){
            if (node->son[0]->datatype != DATATYPE_BOOL){
              fprintf(stderr, "Semantic ERROR: incompatible type for logical operation at line %d.\n", node->line + 1);
              semanticErrors++;
            }
        }
        else if (node->son[0]->datatype != DATATYPE_BOOL || node->son[1]->datatype != DATATYPE_BOOL){
              fprintf(stderr, "Semantic ERROR: incompatible types for logical operation at line %d.\n", node->line + 1);
              semanticErrors++;
        }
        node->datatype = DATATYPE_BOOL;
    }
}


void checkUndeclared() {
    semanticErrors += hashCheckUndeclared();
}

AST *getFunctionDeclaration(char *funcName, AST *node){
    if(node->symbol != NULL && node->type == AST_FUNC && strcmp(node->symbol->text, funcName) == 0)
        return node;

    for(int i = 0; i < MAX_SONS; i++){
        if(node->son[i] == NULL)
            return NULL;
        AST * funDec = getFunctionDeclaration(funcName, node->son[i]);
        if(funDec != NULL)
            return funDec;
    }
    return NULL;
}

int getNumberOfArgumentsCalled(AST *node){
    if(node == NULL) return 0;
    if(node->son[0] != NULL){
        if(node->type == AST_EPARAM)
            return 1 + getNumberOfArgumentsCalled(node->son[1]);
        else
            return getNumberOfArgumentsCalled(node->son[0]);
        
    }
    else
        return 0;
}

int getNumberOfArgumentsDecl(AST *node){
    if(node == NULL) return 0;
    if(node->son[0] != NULL){
        if(node->type == AST_LPARAM){
            return 1 + getNumberOfArgumentsDecl(node->son[1]);    
        } else
            return getNumberOfArgumentsDecl(node->son[0]);
    }
    else
        return 0;
}

int numberOfArgumentsMatch(AST *decl, AST *called){
    int numberOfArgsDecl = getNumberOfArgumentsDecl(decl->son[1]);
    int numberOfArgsCalled = getNumberOfArgumentsCalled(called->son[0]);
    if(numberOfArgsDecl != numberOfArgsCalled){
        fprintf(stderr, "Semantic ERROR in line %d. Incompatible number of arguments in function call. Expected %i arguments but %d called.\n", called->line + 1, numberOfArgsDecl, numberOfArgsCalled);
        semanticErrors++;
        return 0;
    }
    return 1;
}

void validateArgsTypes(AST* node, AST* declaration) {

    if (node->son[0] != NULL && declaration->son[0] != NULL)
    {
        if (!areCompatible(node->son[0]->datatype, declaration->son[0]->symbol->datatype))
        {
            fprintf(stderr, "Semantic ERROR in line %d: Argument type is not compatible.\n", node->line + 1);
            semanticErrors++;
        }
        if (node->son[0]->type == AST_SYMBOL)
        {
            if (node->son[0]->symbol->type == SYMBOL_FUNC)
            {
                fprintf(stderr, "Semantic ERROR in line %d: Cannot use function as argument.\n", node->line + 1);
                semanticErrors++;
            }
            else if (node->son[0]->symbol->type == SYMBOL_VECTOR)
            {
                fprintf(stderr, "Semantic ERROR in line %d: Cannot use vector as argument.\n", node->line + 1);
                semanticErrors++;
            }
        }
        if (node->son[1] != NULL && declaration->son[1] != NULL)
            validateArgsTypes(node->son[1], declaration->son[1]);
    }

}

void validateFunc(AST *node) {
    AST* declaration = getFunctionDeclaration(node->symbol->text, root);
    if(declaration == NULL){
        fprintf(stderr, "Semantic ERROR in line %d: Undeclared function.\n", node->line + 1);
        semanticErrors++;
    } 
    else if(numberOfArgumentsMatch(declaration, node)){
           validateArgsTypes(node->son[0], declaration->son[1]);
    }
    
}

void validatePrint(AST *node){
    if (node == NULL) return;

    if(node->type == AST_SYMBOL){
        if(node->symbol->type == SYMBOL_FUNC){
            fprintf(stderr, "Semantic ERROR: cannot print function at line %d.\n", node->line + 1);
            semanticErrors++;
        }
        else if(node->symbol->type == SYMBOL_VECTOR){
            fprintf(stderr, "Semantic ERROR: cannot print vector at line %d.\n", node->line + 1);
            semanticErrors++;
        }
    }
    validatePrint(node->son[1]);
}

void validateFor(AST *node) {
    if(!isIntType(node->symbol->datatype)){
        fprintf(stderr, "Semantic ERROR: indentifier must be integer at line %d.\n", node->line + 1);
        semanticErrors++;
    }
    if(!isIntType(node->son[0]->datatype)){
        fprintf(stderr, "Semantic ERROR: exp must be integer at line %d.\n", node->line + 1);
        semanticErrors++;
    }
    if(!isIntType(node->son[1]->datatype)){
       fprintf(stderr, "Semantic ERROR: exp must be integer at line %d.\n", node->line + 1);
        semanticErrors++;
    }
    if(!isIntType(node->son[2]->datatype)){
       fprintf(stderr, "Semantic ERROR: exp must be integer at line %d.\n", node->line + 1);
        semanticErrors++;
    }
}


void checkOperands(AST *node) {
    if(!node) return;

    switch(node->type){
        case AST_IDEXP:
            validateFunc(node);
            break;

        case AST_WHILE:
        case AST_IF:
        case AST_IFELSE:
            //node->datatype = DATATYPE_BOOL;
            if(node->son[0]->datatype != DATATYPE_BOOL){
                fprintf(stderr,"Semantic ERROR in line %d. Conditional operation. Operand must be bool.\n", node->line + 1);
                semanticErrors++;
            }
            break;

        case AST_PRINT:
            validatePrint(node->son[0]);
            break;

        case AST_READ:
            if(node->symbol->type != SYMBOL_SCALAR){
                fprintf(stderr,"Semantic ERROR in line %d. Read command expected scalar.\n", node->line + 1);
                semanticErrors++;
            }
            break;

        case AST_VECEXP:

            if (node->symbol->type != SYMBOL_VECTOR){
                fprintf(stderr,"Semantic ERROR in line %d. Index only allowed on vectors.\n", node->line + 1);
                semanticErrors++;
            }
            if (!areCompatible(node->symbol->datatype, node->son[1]->datatype)){
                fprintf(stderr,"Semantic ERROR in line %d. Incompatible type on assigment.\n", node->line + 1);
                semanticErrors++;
            }
            if (!isNumericType(node->son[0]->datatype)){
               fprintf(stderr,"Semantic ERROR in line %d. Index must be numeric.\n", node->line + 1);
                semanticErrors++;
            }
            break;

        //case AST_SYMBOL:
        //    node->datatype = node->symbol->datatype;
        //    break;

        case AST_FOR:
            //node->datatype = DATATYPE_INT;
            validateFor(node);
            break;

       case AST_ASS:
            if (node->symbol->type != SYMBOL_SCALAR){
                fprintf(stderr,"Semantic ERROR in line %d. Assigment must use scalar \n", node->line + 1);
                semanticErrors++;
            }
            if (!areCompatible(node->symbol->datatype, node->son[0]->datatype)){
                fprintf(stderr,"Semantic ERROR in line %d. Incompatible types on assigment\n", node->line + 1);
                semanticErrors++;
            }
            break;


    }

    for(int i = 0; i < MAX_SONS; i++){
        checkOperands(node->son[i]);
    }
}

void checkSemantics(AST *root) {
    fprintf(stderr, "---Checking semantics---\n");
    checkAndSetTypes(root);
    setNodeTypes(root);
    checkUndeclared();    
    checkOperands(root);
    checkReturns(root);
    if(semanticErrors > 0){
        fprintf(stderr, "%d semantic errors!\n", semanticErrors);
        exit(4);
    }
}
