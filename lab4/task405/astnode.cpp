#include "astnode.h"

extern int spaces;
extern std::unique_ptr<LLVMContext> theContext;
extern std::unique_ptr<Module> theModule;
extern std::unique_ptr<IRBuilder<>> builder;
extern std::map<std::string, AllocaInst *> namedValues;
extern std::unique_ptr<legacy::FunctionPassManager> theFPM;
extern int grammererror;
extern std::map<std::string, AllocaInst *> curNamedValues;

//new
struct LocalVarTable {
    int level;
    std::map<std::string, AllocaInst*> localVar;
};
std::vector<LocalVarTable> varTable;
std::vector<LocalVarTable>::iterator varIte;
int level = -1;
bool funWithArg = false;
int findVar(std::string name) {
    for (varIte = varTable.end() - 1; varIte >= varTable.begin(); varIte--) {
        if (varIte->localVar[name]) { return varIte->level; }
    }
    return -1;
}
//new

extern BasicBlock *continueBasicBlock;
void printspaces() {
  for (int i = 0; i < spaces; ++i)
    std::cout << " ";
}
void printGrammerInfo(std::string nodeName, int line) {
  printspaces();
  std::cout << nodeName << " (" << line << ")" << std::endl;
}

void printSemanticError(int type, int line, std::string info = "") {
  grammererror = 1;
  std::cout << "Error type " << type << " at Line " << line << "."
            << std::endl;
}

int parseNIdentifier(NIdentifier &nIdentifier) {
  printspaces();
  std::cout << "ID: " << nIdentifier.name << std::endl;
  return 0;
}

Value *LogErrorV(const char *Str) {
  // std::cout << Str << std::endl;
  return nullptr;
}

void InitializeModuleAndPassManager() {
  // Open a new module.
  theContext = std::make_unique<LLVMContext>();
  theModule = std::make_unique<Module>("test", *theContext);

  // theModule->setDataLayout(dL);

  // Create a new builder for the module.
  builder = std::make_unique<IRBuilder<>>(*theContext);

  // Create a new pass manager attached to it.
  theFPM = std::make_unique<legacy::FunctionPassManager>(theModule.get());

  // Promote allocas to registers.
  //theFPM->add(createPromoteMemoryToRegisterPass());
  // Do simple "peephole" optimizations and bit-twiddling optzns.
  //theFPM->add(createInstructionCombiningPass());
  // Reassociate expressions.
  //theFPM->add(createReassociatePass());
  // Eliminate Common SubExpressions.
  //theFPM->add(createGVNPass());
  // Simplify the control flow graph (deleting unreachable blocks, etc).
  //theFPM->add(createCFGSimplificationPass());

  theFPM->doInitialization();
}

Function *getFunction(std::string Name) {
  // First, see if the function has already been added to the current module.
  if (auto *F = theModule->getFunction(Name))
    return F;
}

/// CreateEntryBlockAlloca - Create an alloca instruction in the entry block
/// of the function.  This is used for mutable variables etc.
static AllocaInst *CreateEntryBlockAlloca(Function *TheFunction,
                                          StringRef VarName, Type *varType) {
  IRBuilder<> TmpB(&TheFunction->getEntryBlock(),
                   TheFunction->getEntryBlock().begin());
  return TmpB.CreateAlloca(varType, nullptr, VarName);
}

int NInteger::parse() {
  printGrammerInfo(getNodeName(), line);
  spaces += 2;
  printspaces();
  std::cout << "INT"
            << ": " << value << std::endl;
  spaces -= 2;
  return 0;
}
int NFloat::parse() {
  printGrammerInfo(getNodeName(), line);
  spaces += 2;
  printspaces();
  std::cout << "FLOAT"
            << ": " << value << std::endl;
  spaces -= 2;
  return 0;
}
int NChar::parse() {
  printGrammerInfo(getNodeName(), line);
  spaces += 2;
  printspaces();
  std::cout << "CHAR"
            << ": " << value << std::endl;
  spaces -= 2;
  return 0;
}
int NIdentifier::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  printspaces();
  std::cout << "ID"
            << ": " << name << std::endl;
  spaces -= 2;
  return 0;
}
int NDotOperator::parse() {
  printGrammerInfo(getNodeName(), line);
  spaces += 2;
  exp.parse();
  printspaces();
  std::cout << "DOT" << std::endl;
  parseNIdentifier(id);
  // id.parse();
  spaces -= 2;
  return 0;
}
int NListOperator::parse() {
  printGrammerInfo(getNodeName(), line);
  spaces += 2;
  lhs.parse();
  printspaces();
  std::cout << "LB" << std::endl;
  rhs.parse();
  printspaces();
  std::cout << "RB" << std::endl;
  spaces -= 2;
  return 0;
}
int NArgs::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  exp.parse();
  if (nArgs) {
    printspaces();
    std::cout << "COMMA" << std::endl;
    nArgs->parse();
  }
  spaces -= 2;
  return 0;
}
int NMethodCall::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  parseNIdentifier(id);
  // id.parse();
  printspaces();
  std::cout << "LP" << std::endl;
  if (nargs) {
    nargs->parse();
  }
  printspaces();
  std::cout << "RP" << std::endl;
  spaces -= 2;
  return 0;
}
int NParenOperator::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  printspaces();
  std::cout << "LP" << std::endl;
  printspaces();
  exp.parse();
  printspaces();
  std::cout << "RP" << std::endl;
  spaces -= 2;
  return 0;
}
int NSingleOperator::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  printspaces();
  std::cout << name << std::endl;
  hs.parse();
  spaces -= 2;
  return 0;
}
int NBinaryOperator::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  lhs.parse();
  printspaces();
  if (name.substr(0, 5) == "RELOP")
    std::cout << "RELOP" << std::endl;
  else
    std::cout << name << std::endl;
  rhs.parse();
  spaces -= 2;
  return 0;
}
int NAssignment::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  lhs.parse();
  printspaces();
  std::cout << name << std::endl;
  rhs.parse();
  spaces -= 2;
  return 0;
}
int NSpecifier::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  printspaces();
  std::cout << "TYPE: " << type << std::endl;
  spaces -= 2;
  return 0;
}
int NVarDec::parse() {
  printGrammerInfo(getNodeName(), line);

  if (v.size()) {
    spaces += 2;
    for (int i = 0; i < v.size(); ++i) {
      printGrammerInfo(getNodeName(), line);

      spaces += 2;
    }
    parseNIdentifier(Id);
    // Id.parse();
    spaces -= 2;
    for (int i = 0; i < v.size(); ++i) {
      printspaces();
      std::cout << "LB" << std::endl;
      printspaces();
      std::cout << "INT: " << v[i] << std::endl;
      printspaces();
      std::cout << "RB" << std::endl;
      spaces -= 2;
    }
  } else {
    spaces += 2;
    parseNIdentifier(Id);
    // Id.parse();
    spaces -= 2;
  }
  return 0;
}
int NParamDec::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  nSpecifier.parse();
  varDec.parse();
  spaces -= 2;
  return 0;
}
int NVarList::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  nParamDec.parse();
  if (nVarList) {
    printspaces();
    std::cout << "COMMA" << std::endl;
    nVarList->parse();
  }
  spaces -= 2;
  return 0;
}
int NFunDec::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  parseNIdentifier(Id);
  // Id.parse();
  printspaces();
  std::cout << "LP" << std::endl;
  if (arguments)
    arguments->parse();
  printspaces();
  std::cout << "RP" << std::endl;
  spaces -= 2;
  return 0;
}
int NDec::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  vardec.parse();
  if (exp) {
    printspaces();
    std::cout << "ASSIGNOP" << std::endl;
    exp->parse();
  }
  spaces -= 2;
  return 0;
}
int NDecList::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  dec.parse();
  if (nDecList) {
    printspaces();
    std::cout << "COMMA" << std::endl;
    nDecList->parse();
  }
  spaces -= 2;
  return 0;
}
int NDef::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  nSpecifier.parse();
  if (nDecList)
    nDecList->parse();
  printspaces();
  std::cout << "SEMI" << std::endl;
  spaces -= 2;
  return 0;
}
int NDefList::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  nDef.parse();
  if (nDefList) {
    nDefList->parse();
  }
  spaces -= 2;
  return 0;
}
int NStructSpecifier::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  printGrammerInfo("StructSpecifier", line);

  spaces += 2;
  printspaces();
  std::cout << "STRUCT" << std::endl;
  if (deflist) {
    if (tag) {
      printGrammerInfo("OptTag", line);
      spaces += 2;
      parseNIdentifier(*tag);
      spaces -= 2;
      printspaces();
      std::cout << "LC" << std::endl;
      deflist->parse();
      printspaces();
      std::cout << "RC" << std::endl;
    } else {
      deflist->parse();
    }
  } else if (tag) {
    printGrammerInfo("Tag", line);

    spaces += 2;
    parseNIdentifier(*tag);
    spaces -= 2;
  }
  spaces -= 2;
  spaces -= 2;
  return 0;
}
int NStmtList::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  nStmt.parse();
  if (nStmtList)
    nStmtList->parse();
  spaces -= 2;
  return 0;
}

int NCompSt::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  printspaces();
  std::cout << "LC" << std::endl;
  if (ndeflist)
    ndeflist->parse();
  if (nstmtlist)
    nstmtlist->parse();
  printspaces();
  std::cout << "RC" << std::endl;
  spaces -= 2;
  return 0;
}
int NExpStmt::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  this->exp.parse();
  printspaces();
  std::cout << "SEMI" << std::endl;
  spaces -= 2;
  return 0;
}
int NCompStStmt::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  compst.parse();
  spaces -= 2;
  return 0;
}
int NRetutnStmt::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  printspaces();
  std::cout << "RETURN" << std::endl;
  this->exp.parse();
  printspaces();
  std::cout << "SEMI" << std::endl;
  spaces -= 2;
  return 0;
}
int NIfStmt::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  printspaces();
  std::cout << "IF" << std::endl;
  printspaces();
  std::cout << "LP" << std::endl;
  this->exp.parse();
  printspaces();
  std::cout << "RP" << std::endl;
  this->stmt.parse();
  spaces -= 2;
  return 0;
}
int NIfElseStmt::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  printspaces();
  std::cout << "IF" << std::endl;
  printspaces();
  std::cout << "LP" << std::endl;
  this->exp.parse();
  printspaces();
  std::cout << "RP" << std::endl;
  this->stmt.parse();
  printspaces();
  std::cout << "ELSE" << std::endl;
  this->stmt_else.parse();
  spaces -= 2;
  return 0;
}
int NWhileStmt::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  printspaces();
  std::cout << "WHILE" << std::endl;
  printspaces();
  std::cout << "LP" << std::endl;
  this->exp.parse();
  printspaces();
  std::cout << "RP" << std::endl;
  this->stmt.parse();
  spaces -= 2;
  return 0;
}
int NBreakStmt::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  printspaces();
  std::cout << "BREAK" << std::endl;
  printspaces();
  std::cout << "SEMI" << std::endl;
  spaces -= 2;
  return 0;
}
int NExtDecList::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  nVarDec.parse();
  if (nExtDecList) {
    printspaces();
    std::cout << "COMMA" << std::endl;
    nExtDecList->parse();
  }
  spaces -= 2;
  return 0;
}
int NExtDef::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  specifier.parse();
  if (fundec) {
    fundec->parse();
    if (compst) {
      compst->parse();
    }
  } else {
    if (nextdeclist) {
      nextdeclist->parse();
    }
    printspaces();
    std::cout << "SEMI" << std::endl;
  }

  spaces -= 2;
  return 0;
}
int NExtDefList::parse() {
  printGrammerInfo(getNodeName(), line);

  spaces += 2;
  nExtDef.parse();
  if (nExtDefList)
    nExtDefList->parse();
  spaces -= 2;
  return 0;
}
int NProgram::parse() {
  printGrammerInfo("Program", line);
  spaces += 2;
  if (nextdeflist)
    nextdeflist->parse();
  spaces -= 2;
  return 0;
}

// codegen()
Value *Node::codegen() {
  assert(false); // Never use this function.
  // This is a list.
  return ConstantInt::get(*theContext, APInt(32, 0, true));
}
Value *NExpression::codegen() {
  return ConstantInt::get(*theContext, APInt(32, 0, true));
}
Value *NInteger::codegen() {
  return ConstantInt::get(*theContext, APInt(32, value, true));
}    
Value *NFloat::codegen() {
  // begin
    return ConstantFP::get(*theContext, APFloat(value));
  return nullptr;
  // end
}
Value *NChar::codegen() {
  // begin
    return ConstantInt::get(*theContext, APInt(32, value, true));
  return nullptr;
  // end
}
Value *NIdentifier::codegen() {
  // begin
    Value* retVal = nullptr;
    int varLevel;
    if ((varLevel = findVar(std::string(name))) != -1) {
        retVal = builder->CreateLoad(varTable[varLevel].localVar[std::string(name)], "load");
    } else {
        printSemanticError(1, line, "Undefined " + name);
    }
    return retVal;
  // end
}
Value *NArgs::codegen() { return exp.codegen(); }
Value *NMethodCall::codegen() {
  // begin
    Value* retVal = nullptr;
    std::vector<Value*> argsV;
    Function* func = theModule->getFunction(id.name);
    if (!func) {
        printSemanticError(2, line, "Undefined Method " + id.name);
    }
    for (auto pArg = nargs; pArg; pArg = pArg->nArgs) {
        argsV.push_back(pArg->codegen());
    }
    if (argsV.size() != func->arg_size()) {
        printSemanticError(8, line, "Wrong arg num.");
    }
    for (std::size_t i = 0; i < argsV.size(); i++) {
        if (argsV[i]->getType() != func->getArg(i)->getType()) {
            printSemanticError(8, line, "Wrong arg type.");
        }
    }
    retVal = builder->CreateCall(func, argsV, "methodCall");
    return retVal;
  // end
}
Value *NParenOperator::codegen() { return exp.codegen(); }
Value *NSingleOperator::codegen() {
  // begin
    Value* retVal = hs.codegen();
    return retVal;
  // end
}
Value *NBinaryOperator::codegen() {
  // begin
    Value* retVal = nullptr;
    Value *tempLHS = lhs.codegen(), *tempRHS = rhs.codegen();
    if (tempLHS == nullptr || tempRHS == nullptr) { return nullptr; }
    if (name == "AND") {
        retVal = builder->CreateAnd(tempLHS, tempRHS, "and");
    } else if (name == "OR") {
        retVal = builder->CreateOr(tempLHS, tempRHS, "or");
    } else if (name == "RELOP==") {
        retVal = builder->CreateICmpEQ(tempLHS, tempRHS, "EQ");
    } else if (name == "RELOP!=") {
        retVal = builder->CreateICmpNE(tempLHS, tempRHS, "NE");
    } else if (name == "RELOP<=") {
        retVal = builder->CreateICmpSLE(tempLHS, tempRHS, "SLE");
    } else if (name == "RELOP>=") {
        retVal = builder->CreateICmpSGE(tempLHS, tempRHS, "SGE");
    } else if (name == "RELOP<") {
        retVal = builder->CreateICmpSLT(tempLHS, tempRHS, "SLT");
    } else if (name == "RELOP>") {
        retVal = builder->CreateICmpSGT(tempLHS, tempRHS, "SGT");
    } else if (name == "PLUS") {
        retVal = builder->CreateAdd(tempLHS, tempRHS, "add");
    } else if (name == "MINUS") {
        retVal = builder->CreateSub(tempLHS, tempRHS, "sub");
    } else if (name == "STAR") {
        retVal = builder->CreateMul(tempLHS, tempRHS, "mul");
    } else if (name == "DIV") {
        retVal = builder->CreateSDiv(tempLHS, tempRHS, "sdiv");
    } else if (name == "MOD") {
        retVal = builder->CreateSRem(tempLHS, tempRHS, "srem");
    }
    return retVal;
  // end
}
Value *NAssignment::codegen() {
  // Assignment requires the LHS to be an identifier.
  // begin
    Value* retVal = nullptr;
    int varLevel;
    if ((varLevel = findVar(std::string(lhs.name))) != -1) {
        Value *tempLHS = lhs.codegen(), *tempRHS = rhs.codegen();
        if (tempLHS == nullptr || tempRHS == nullptr) return nullptr;
        if (tempLHS->getType() != tempRHS->getType()) {
            printSemanticError(5, line, "Type error in binary operator.");
        }
        if (this->name == "ASSIGNOP") {
            builder->CreateStore((retVal = tempRHS), varTable[varLevel].localVar[std::string(lhs.name)]);
        } else if (this->name == "PLUSASS") {
            retVal = builder->CreateAdd(tempLHS, tempRHS, "add");
            builder->CreateStore(retVal, varTable[varLevel].localVar[std::string(lhs.name)]);
        } else if (this->name == "MINUSASS") {
            retVal = builder->CreateSub(tempLHS, tempRHS, "sub");
            builder->CreateStore(retVal, varTable[varLevel].localVar[std::string(lhs.name)]);
        } else if (this->name == "STARASS") {
            retVal = builder->CreateMul(tempLHS, tempRHS, "mul");
            builder->CreateStore(retVal, varTable[varLevel].localVar[std::string(lhs.name)]);
        } else if (this->name == "DIVASS") {
            retVal = builder->CreateSDiv(tempLHS, tempRHS, "sdiv");
            builder->CreateStore(retVal, varTable[varLevel].localVar[std::string(lhs.name)]);
        }
    } 
    else {
        printSemanticError(6, line, "Only rvalue " + lhs.name);
    }
    return retVal;
  // end
}
Value *NSpecifier::codegen() {
  // begin
    if (type == "int")
        return ConstantInt::get(*theContext, APInt(32, 0, true));
    if (type == "float")
        return ConstantFP::get(*theContext, APFloat(0.0));
    if (type == "char")
        return ConstantInt::get(*theContext, APInt(8, 0, false));
    assert(false);
    return ConstantInt::get(*theContext, APInt(32, 0, true));
    // end
}
Type *NSpecifier::getType() {
  if (type == "int")
    return Type::getInt32Ty(*theContext);
  if (type == "float")
    return Type::getFloatTy(*theContext);
  if (type == "char")
    return Type::getInt8Ty(*theContext);
  assert(false);
  return Type::getInt32Ty(*theContext);
}
Value* NVarDec::codegen(Type* varType, Value* varValue) {
  // begin
    Value* retVal = nullptr;
    AllocaInst* alloca = builder->CreateAlloca(varType, nullptr, Id.name);
    if (varTable[level].localVar[Id.name]) {
        printSemanticError(3, line, "Redefined " + Id.name);
    }
    if (!varValue) { varValue = ConstantInt::get(*theContext, APInt(32, 0, true)); }
    builder->CreateStore(varValue, alloca);
    retVal = varValue;
    varTable[level].localVar[std::string(Id.name)] = alloca;
    return retVal;
  // end
}
Value *NParamDec::codegen() {
  // begin
    return ConstantInt::get(*theContext, APInt(32, 0, true));
  // end
}

std::pair<std::string, Type *> NParamDec::getType() {
  assert(varDec.v.size() == 0);
  std::pair<std::string, Type *> tmp(varDec.Id.name, nSpecifier.getType());
  return tmp;
}
Value *NVarList::codegen() {
  assert(false); // Never use this function.
  // This is a list.
  return ConstantInt::get(*theContext, APInt(32, 0, true));
}
Function *NFunDec::funcodegen(Type *retType) {
  // check if it exists the same name of fun
  if (theModule->getFunction(Id.name)) {
    printSemanticError(4, line, "Redefined " + Id.name);
    return nullptr;
  }

  std::vector<Type *> argsTypes;
  std::vector<std::string> argNames;
  for (NVarList *item = arguments; item; item = item->nVarList) {
    funWithArg = true;
    auto tmp = item->nParamDec.getType();
    argNames.push_back(tmp.first);
    argsTypes.push_back(tmp.second);
  }

  if (funWithArg) {
      level++;
      LocalVarTable localVarTable = {0};
      localVarTable.level = level;
      varTable.push_back(localVarTable);
  }

  FunctionType *ft = FunctionType::get(retType, argsTypes, false);
  Function *f =
      Function::Create(ft, Function::ExternalLinkage, Id.name, theModule.get());
  unsigned idx = 0;
  for (auto &arg : f->args()) {
    arg.setName(argNames[idx++]);
  }
  return f;
}

Value* NDec::codegen(Type* varType) {  // new
    // begin
    Value* retVal = nullptr;
    if (!exp) {
        retVal = vardec.codegen(varType, nullptr);
    } else {
        retVal = vardec.codegen(varType, exp->codegen());
    }
    return retVal;
    // 
}

Value* NDecList::codegen(Type* varType) {  // new

    // begin
    Value* retVal = nullptr;
    retVal = dec.codegen(varType);
    if (!retVal) {
        return nullptr;
    }
    if (nDecList) {
        retVal = nDecList->codegen(varType);
    }
    return retVal;
    // end
}


Value *NDef::codegen() {
  // begin
    Type* varType = nSpecifier.getType();
    Value* retVal = nDecList->codegen(varType);
    return retVal;
  // end
}
Value *NDefList::codegen() {
  // begin
    Value* retVal = nDef.codegen();
    if (!retVal) { return nullptr; }
    if (nDefList) { retVal = nDefList->codegen(); }
    return retVal;
  // end
}
Value *NStmtList::codegen() {
  auto *retVal = nStmt.codegen();
  if (nStmtList)
    retVal = nStmtList->codegen();
  return retVal;
}
Value *NCompSt::codegen() {
  // 自行处理变量作用域的问题
  Value *retVal = nullptr;
      if (!funWithArg) {
        level++;
        LocalVarTable localVarTable = {0};
        localVarTable.level = level;
        varTable.push_back(localVarTable);
    } else {
        funWithArg = false;
    }
  if (ndeflist)
    retVal = ndeflist->codegen();
  if (nstmtlist)
    retVal = nstmtlist->codegen();
  varTable.pop_back();
  level--;
  return retVal;
}
Value *NExpStmt::codegen() { return exp.codegen(); }
Value *NCompStStmt::codegen() {
  // begin
    return compst.codegen();
  // end
}
Value *NRetutnStmt::codegen() {
  Function *theFun = builder->GetInsertBlock()->getParent();
  BasicBlock *bb = BasicBlock::Create(*theContext, "ret", theFun);
  builder->CreateBr(bb);
  builder->SetInsertPoint(bb);
  auto *retVal = exp.codegen();
  // check the return type and fundec type
  // begin
    if (!retVal) { return nullptr; }
    if (retVal->getType() != theFun->getReturnType()) {
        printSemanticError(7, line, "Wrong return type.");
        return LogErrorV("Wrong return type.");
    }
  // end
    builder->CreateRet(retVal);
  return retVal;
}
Value *NIfStmt::codegen() {
  Function *theFun = builder->GetInsertBlock()->getParent();
  // begin
    Value* retVal = nullptr;
    Value* compI = exp.codegen();
    Value* condValI = builder->CreateICmpNE(compI, Constant::getNullValue(compI->getType()), "condValI");
    // 创建条件为真和假应跳转的2个基本块
    BasicBlock* thenBlockI = BasicBlock::Create(*theContext, "thenI", theFun);
    BasicBlock* contBlockI = BasicBlock::Create(*theContext, "contI", theFun);
    // 根据condValI值跳转 真为thenBlockI 否则为contBlockI
    builder->CreateCondBr(condValI, thenBlockI, contBlockI);
    // 进入thenBlockI基本块
    builder->SetInsertPoint(thenBlockI);
    retVal = ConstantInt::get(*theContext, APInt(32, 1, true));
    stmt.codegen();
    builder->CreateBr(contBlockI);
    // 进入 contBlockI
    builder->SetInsertPoint(contBlockI);
    if (!retVal) { retVal = ConstantInt::get(*theContext, APInt(32, 0, true)); }
    return retVal;
  // end
}
Value *NIfElseStmt::codegen() {
  Function *theFun = builder->GetInsertBlock()->getParent();
  // begin
    Value* retVal = nullptr;
    Value* compIE = exp.codegen();
    Value* condValIE = builder->CreateICmpNE(compIE, Constant::getNullValue(compIE->getType()), "condValIE");
    // 创建条件为真和假应跳转的3个基本块
    BasicBlock* thenBlockIE = BasicBlock::Create(*theContext, "thenIE", theFun);
    BasicBlock* elseBlockIE = BasicBlock::Create(*theContext, "elseIE", theFun);
    BasicBlock* contBlockIE = BasicBlock::Create(*theContext, "contIE", theFun);
    // 根据condValIE值跳转 真为thenBlockIE 否则为elseBlockIE
    builder->CreateCondBr(condValIE, thenBlockIE, elseBlockIE);
    // 进入thenBlockIE基本块
    builder->SetInsertPoint(thenBlockIE);
    retVal = ConstantInt::get(*theContext, APInt(32, 0, true));
    stmt.codegen();
    builder->CreateBr(contBlockIE);
    // 进入elseBlockIE基本块
    builder->SetInsertPoint(elseBlockIE);
    retVal = ConstantInt::get(*theContext, APInt(32, 1, true));
    stmt_else.codegen();
    builder->CreateBr(contBlockIE);
    // 进入 contBlockIE
    builder->SetInsertPoint(contBlockIE);
    return retVal;
  // end
}
Value *NWhileStmt::codegen() {
  Function *theFun = builder->GetInsertBlock()->getParent();
  BasicBlock *condb = BasicBlock::Create(*theContext, "cond", theFun);
  // begin
    Value* retVal;
    BasicBlock* doBlockW = BasicBlock::Create(*theContext, "doW", theFun);
    BasicBlock* ntBlockW = BasicBlock::Create(*theContext, "ntW", theFun);
    // **
    builder->CreateBr(condb);
    // 进入condb基本块
    builder->SetInsertPoint(condb);
    Value* compW = exp.codegen();
    Value* condValW = builder->CreateICmpNE(compW, Constant::getNullValue(compW->getType()), "condValW");
    // 根据condValW值跳转 真为doBlockW 否则为ntBlockW
    builder->CreateCondBr(condValW, doBlockW, ntBlockW);
    // 进入doBlockW基本块
    builder->SetInsertPoint(doBlockW);
    stmt.codegen();
    builder->CreateBr(condb);
    // 进入 ntBlockW
    builder->SetInsertPoint(ntBlockW);
    return ConstantInt::get(*theContext, APInt(32, 0, true));
  // end
}
Value *NBreakStmt::codegen() {
  // begin

    return ConstantInt::get(*theContext, APInt(32, 0, true));

  // end
}
Value *NExtDefVarDec::codegen() {
  // begin

    return ConstantInt::get(*theContext, APInt(32, 0, true));

  // end
}
Value *NExtDefFunDec::codegen() {
  Type *retType = specifier.getType();

  Function *f = fundec->funcodegen(retType);
  if (!f) {
    return nullptr;
  }
  assert(compst != nullptr); // Assert compst is not null.
  BasicBlock *bb = BasicBlock::Create(*theContext, "entry", f);
  builder->SetInsertPoint(bb);
  namedValues.clear();
  for (auto &arg : f->args()) {
    // Create an alloca for this variable.
    AllocaInst *alloca =
        CreateEntryBlockAlloca(f, arg.getName(), arg.getType());

    if (findVar(std::string(arg.getName())) != -1) {
            printSemanticError(3, line, "Redefined " + arg.getName().str());
            return LogErrorV("Unknown function referenced");
        }
        // Store the initial value into the alloca.
        builder->CreateStore(&arg, alloca);
        // Add arguments to variable symbol table.
        // namedValues[std::string(arg.getName())] = alloca;
        // curNamedValues[std::string(arg.getName())] = alloca;
        varTable[level].localVar[std::string(arg.getName())] = alloca;
  }
  if (Value *retVal = compst->codegen()) {
    // Finish off the function.

    // Validate the generated code, checking for consistency.
    verifyFunction(*f);

    // Run the optimizer on the function.
    // theFPM->run(*f);
    return f;
  }
  // Error reading body, remove function.
  f->eraseFromParent();

  return nullptr;
}
Value *NExtDefList::codegen() {
  auto *lastCode = nExtDef.codegen();
  // lastCode->print(errs());
  // assert(nExtDefList == nullptr);
  if (nExtDefList)
    lastCode = nExtDefList->codegen();
  return lastCode;
}
Value *NProgram::codegen() {

  //默认输出函数putchar
  std::vector<Type *> putArgs;
  putArgs.push_back(Type::getInt32Ty(*theContext));

  FunctionType *putType =
      FunctionType::get(builder->getInt32Ty(), putArgs, false);
  Function *putFunc = Function::Create(putType, Function::ExternalLinkage,
                                       "putchar", theModule.get());

  //默认输入函数getchar
  std::vector<Type *> getArgs;
  // getArgs.push_back(Type::getInt32Ty(*theContext));

  FunctionType *getType =
      FunctionType::get(builder->getInt32Ty(), getArgs, false);
  Function *getFunc = Function::Create(getType, Function::ExternalLinkage,
                                       "getchar", theModule.get());

  Value *lastCode = nextdeflist->codegen();
  if (grammererror)
    return nullptr;
  return lastCode;
}

