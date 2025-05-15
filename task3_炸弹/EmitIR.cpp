/**
 * @file EmitIR.cpp
 * @brief LLVM IR生成器实现
 */
#include "EmitIR.hpp"
#include <llvm/Transforms/Utils/ModuleUtils.h>
#include <iostream>

using namespace asg;

EmitIR::EmitIR(Obj::Mgr& mgr, llvm::LLVMContext& ctx, llvm::StringRef mid)
  : mMgr(mgr)
  , mMod(mid, ctx)
  , mCtx(ctx)
  , mIntTy(llvm::Type::getInt32Ty(ctx))
  , mCtorTy(llvm::FunctionType::get(llvm::Type::getVoidTy(ctx), false))
  , mCurIrb(std::make_unique<llvm::IRBuilder<>>(ctx))
{
  // 设置源文件名属性
  mMod.setSourceFileName(mid.str());
}

llvm::Module& EmitIR::operator()(TranslationUnit* tu) {
    // 分两阶段处理声明，确保能正确处理前向引用
    
    // 1. 首先处理全局变量声明，生成符号表条目
    for (auto&& decl : tu->decls) {
        if (auto varDecl = decl->dcst<VarDecl>()) {
            (*this)(varDecl);
        }
    }
    
    // 2. 然后处理函数定义
    for (auto&& decl : tu->decls) {
        if (auto funcDecl = decl->dcst<FunctionDecl>()) {
            (*this)(funcDecl);
        }
    }
    
    return mMod;
}

//==============================================================================
// 辅助工具函数实现
//==============================================================================

void EmitIR::createGlobalCtor(const std::string& varName, 
                              asg::Expr* initExpr, 
                              llvm::GlobalVariable* globalVar) {
    auto& irb = *mCurIrb;
    
    // 创建全局构造函数来初始化
    std::string ctorName = "ctor_" + varName;
    auto ctorFunc = llvm::Function::Create(
        mCtorTy, 
        llvm::GlobalValue::PrivateLinkage,
        ctorName, 
        mMod
    );
    
    // 创建构造函数体
    auto entryBB = llvm::BasicBlock::Create(mCtx, "entry", ctorFunc);
    auto savedFunc = mCurFunc;
    auto savedBlock = irb.GetInsertBlock();
    
    // 切换到构造函数中
    irb.SetInsertPoint(entryBB);
    mCurFunc = ctorFunc;
    
    // 计算初始化表达式并存储到全局变量
    auto initVal = (*this)(initExpr);
    
    // 确保类型匹配
    initVal = ensureType(initVal, globalVar->getValueType());
    
    irb.CreateStore(initVal, globalVar);
    irb.CreateRetVoid();
    
    // 添加到全局构造函数列表
    llvm::appendToGlobalCtors(mMod, ctorFunc, 65535);
    
    // 恢复原来的上下文
    mCurFunc = savedFunc;
    if (savedBlock) {
        irb.SetInsertPoint(savedBlock);
    }
}

llvm::Value* EmitIR::ensureType(llvm::Value* value, llvm::Type* targetType) {
    auto& irb = *mCurIrb;
    
    if (value->getType() == targetType) {
        return value; // 类型已经匹配
    }
    
    // 整数类型之间的转换
    if (targetType->isIntegerTy() && value->getType()->isIntegerTy()) {
        auto destBits = targetType->getIntegerBitWidth();
        auto srcBits = value->getType()->getIntegerBitWidth();
        
        if (destBits > srcBits) {
            return irb.CreateSExt(value, targetType);
        } else if (destBits < srcBits) {
            return irb.CreateTrunc(value, targetType);
        }
    }
    
    // 指针类型之间的转换
    if (targetType->isPointerTy() && value->getType()->isPointerTy()) {
        return irb.CreateBitCast(value, targetType);
    }
    
    // 其他复杂类型转换逻辑可以按需添加
    
    // 无法处理的类型转换，返回原始值并记录警告
    std::cerr << "Warning: Unable to convert between incompatible types in ensureType" << std::endl;
    return value;
}

//==============================================================================
// 类型处理
//==============================================================================

llvm::Type* EmitIR::operator()(const Type* type) {
    // 基本类型处理（没有类型表达式修饰）
    if (type->texp == nullptr) {
        switch (type->spec) {
            case Type::Spec::kInt:
                return llvm::Type::getInt32Ty(mCtx);
            case Type::Spec::kVoid:
                return llvm::Type::getVoidTy(mCtx);
            case Type::Spec::kChar:
                return llvm::Type::getInt8Ty(mCtx);
            case Type::Spec::kLong:
                return llvm::Type::getInt32Ty(mCtx);
            case Type::Spec::kLongLong:
                return llvm::Type::getInt64Ty(mCtx);
            default:
                std::cerr << "Error: Unknown type specifier" << std::endl;
                ABORT();
        }
    }

    // 创建子类型
    Type subType;
    subType.spec = type->spec;
    subType.qual = type->qual;
    subType.texp = type->texp->sub;

    // 类型表达式处理
    if (auto pointerType = type->texp->dcst<PointerType>()) {
        // 使用不透明指针 (Opaque Pointers) - LLVM 17+
        return llvm::PointerType::get(mCtx, 0);
    }
    else if (auto arrayType = type->texp->dcst<ArrayType>()) {
        llvm::Type* elemType = (*this)(&subType);
        return llvm::ArrayType::get(elemType, arrayType->len);
    }
    else if (auto functionType = type->texp->dcst<FunctionType>()) {
        std::vector<llvm::Type*> paramTypes;
        for (const Type* paramType : functionType->params) {
            paramTypes.push_back((*this)(paramType));
        }
        return llvm::FunctionType::get((*this)(&subType), paramTypes, false);
    }

    std::cerr << "Error: Unsupported type expression" << std::endl;
    ABORT();
}

//==============================================================================
// 表达式处理
//==============================================================================

llvm::Value* EmitIR::operator()(Expr* obj) {
    if (!obj) {
        std::cerr << "Error: Null expression encountered" << std::endl;
        ABORT();
    }
    
    // 分发到相应的处理函数
    if (auto p = obj->dcst<ImplicitInitExpr>())
        return (*this)(p);
    if (auto p = obj->dcst<IntegerLiteral>())
        return (*this)(p);
    if (auto p = obj->dcst<DeclRefExpr>())
        return (*this)(p);
    if (auto p = obj->dcst<ParenExpr>())
        return (*this)(p);
    if (auto p = obj->dcst<UnaryExpr>())
        return (*this)(p);
    if (auto p = obj->dcst<BinaryExpr>())
        return (*this)(p);
    if (auto p = obj->dcst<CallExpr>())
        return (*this)(p);
    if (auto p = obj->dcst<InitListExpr>())
        return (*this)(p);
    if (auto p = obj->dcst<ImplicitCastExpr>())
        return (*this)(p);
    if (auto p = obj->dcst<StringLiteral>())
        return (*this)(p);
    
    // 未知表达式类型
    std::cerr << "Error: Unknown expression type: " << typeid(*obj).name() << std::endl;
    ABORT();
}

llvm::Value* EmitIR::operator()(ImplicitInitExpr* obj) {
    // 隐式初始化表达式，返回整数0
    return llvm::ConstantInt::get(llvm::Type::getInt32Ty(mCtx), 0);
}

llvm::Constant* EmitIR::operator()(IntegerLiteral* obj) {
    // 整数字面值，根据类型创建常量
    return llvm::ConstantInt::get((*this)(obj->type), obj->val);
}

llvm::Value* EmitIR::operator()(StringLiteral* obj) {
    // 字符串字面值，创建全局字符串指针
    return mCurIrb->CreateGlobalStringPtr(obj->val);
}

llvm::Value* EmitIR::operator()(DeclRefExpr* obj) {
    // 处理函数引用
    if (auto funcDecl = obj->decl->dcst<FunctionDecl>()) {
        auto func = mMod.getFunction(funcDecl->name);
        if (!func) {
            // 如果函数尚未定义，创建函数声明
            func = llvm::Function::Create(
                llvm::cast<llvm::FunctionType>((*this)(funcDecl->type)), 
                llvm::GlobalVariable::ExternalLinkage,
                funcDecl->name, mMod);
        }
        return func;
    }
    
    // 处理变量引用
    if (auto varDecl = obj->decl->dcst<VarDecl>()) {
        if (varDecl->any) {
            // 变量已经在作用域内，返回其地址
            return static_cast<llvm::Value*>(varDecl->any);
        }
    }
    
    std::cerr << "Error: Referenced declaration not found or invalid: " 
              << (obj->decl ? obj->decl->name : "unnamed") << std::endl;
    ABORT();
}

llvm::Value* EmitIR::operator()(ParenExpr* obj) {
    // 括号表达式，直接返回子表达式的值
    return (*this)(obj->sub);
}

llvm::Value* EmitIR::operator()(UnaryExpr* obj) {
    auto& irb = *mCurIrb;
    
    // 处理一元运算符
    switch (obj->op) {
        case UnaryExpr::kPos:
            // 正号运算符，不改变值
            return (*this)(obj->sub);
            
        case UnaryExpr::kNeg: {
            // 负号运算符
            auto val = (*this)(obj->sub);
            return irb.CreateNeg(val);
        }
            
        case UnaryExpr::kNot: {
            // 逻辑非运算符
            auto val = (*this)(obj->sub);
            
            // 如果已是布尔值(i1)，直接取非
            if (val->getType()->isIntegerTy(1)) {
                return irb.CreateNot(val);
            }
            
            // 否则先转为布尔值再取非
            auto cmp = irb.CreateICmpNE(val, 
                llvm::ConstantInt::get(val->getType(), 0));
            return irb.CreateNot(cmp);
        }
            
        default:
            std::cerr << "Error: Unknown unary operator: " << obj->op << std::endl;
            ABORT();
    }
}

void EmitIR::emitCondBr(asg::Expr* cond, llvm::BasicBlock* trueBB, llvm::BasicBlock* falseBB) {
    auto& irb = *mCurIrb;
    
    // 特殊处理逻辑与运算，实现短路求值
    if (auto bin = cond->dcst<asg::BinaryExpr>()) {
        if (bin->op == asg::BinaryExpr::kAnd) {
            // 处理 && 运算符：左假则假，左真则看右
            auto func = irb.GetInsertBlock()->getParent();
            auto rhsBB = llvm::BasicBlock::Create(mCtx, "land.rhs", func);
            
            // 先计算左操作数
            emitCondBr(bin->lft, rhsBB, falseBB);
            
            // 然后计算右操作数
            irb.SetInsertPoint(rhsBB);
            emitCondBr(bin->rht, trueBB, falseBB);
            return;
        }
        
        // 特殊处理逻辑或运算，实现短路求值
        if (bin->op == asg::BinaryExpr::kOr) {
            // 处理 || 运算符：左真则真，左假则看右
            auto func = irb.GetInsertBlock()->getParent();
            auto rhsBB = llvm::BasicBlock::Create(mCtx, "lor.rhs", func);
            
            // 先计算左操作数
            emitCondBr(bin->lft, trueBB, rhsBB);
            
            // 然后计算右操作数
            irb.SetInsertPoint(rhsBB);
            emitCondBr(bin->rht, trueBB, falseBB);
            return;
        }
    }
    
    // 处理一般条件表达式
    auto val = (*this)(cond);
    
    // 确保条件是布尔类型(i1)
    if (!val->getType()->isIntegerTy(1)) {
        val = irb.CreateICmpNE(val, llvm::ConstantInt::get(val->getType(), 0));
    }
    
    // 创建条件分支
    irb.CreateCondBr(val, trueBB, falseBB);
}

llvm::Value* EmitIR::operator()(BinaryExpr* obj) {
    auto& irb = *mCurIrb;

    // 特殊处理逗号表达式
    if (obj->op == BinaryExpr::kComma) {
        (*this)(obj->lft); // 计算左侧，忽略结果
        return (*this)(obj->rht); // 返回右侧值
    }

    // 特殊处理赋值表达式
    if (obj->op == BinaryExpr::kAssign) {
        auto lhs = (*this)(obj->lft); // 获取左侧地址
        auto rhs = (*this)(obj->rht); // 获取右侧值
        
        // 类型转换确保兼容性
        // 修复后:
if (auto ptrType = llvm::dyn_cast<llvm::PointerType>(lhs->getType())) {
    // 如果我们有额外的类型信息，可以从AST获取
    if (obj->lft->type && obj->lft->type->texp) {
        if (auto ptrAstType = obj->lft->type->texp->dcst<PointerType>()) {
            // 从AST获取元素类型
            Type elemAstType;
            elemAstType.spec = obj->lft->type->spec;
            elemAstType.qual = obj->lft->type->qual;
            elemAstType.texp = ptrAstType->sub;
            auto elemType = (*this)(&elemAstType);
            
            // 确保右值类型匹配
            rhs = ensureType(rhs, elemType);
        }
    }
}

        
        // 执行存储操作并返回右值
        irb.CreateStore(rhs, lhs);
        return rhs;
    }

    // 特殊处理逻辑与运算，实现短路求值
    if (obj->op == BinaryExpr::kAnd) {
        auto func = irb.GetInsertBlock()->getParent();
        auto lhsBB = irb.GetInsertBlock();
        auto rhsBB = llvm::BasicBlock::Create(mCtx, "land.rhs", func);
        auto endBB = llvm::BasicBlock::Create(mCtx, "land.end", func);

        // 计算左操作数
        auto lhsVal = (*this)(obj->lft);
        llvm::Value* lhsCond = lhsVal;
        
        // 确保比较的是布尔值
        if (!lhsCond->getType()->isIntegerTy(1)) {
            lhsCond = irb.CreateICmpNE(lhsCond, 
                llvm::ConstantInt::get(lhsCond->getType(), 0));
        }

        // 创建条件分支：如果左操作数为假，则短路到结果为假；否则，计算右操作数
        irb.CreateCondBr(lhsCond, rhsBB, endBB);

        // 计算右操作数
        irb.SetInsertPoint(rhsBB);
        auto rhsVal = (*this)(obj->rht);
        llvm::Value* rhsCond = rhsVal;
        
        // 确保比较的是布尔值
        if (!rhsCond->getType()->isIntegerTy(1)) {
            rhsCond = irb.CreateICmpNE(rhsCond, 
                llvm::ConstantInt::get(rhsCond->getType(), 0));
        }
        
        auto rhsEndBB = irb.GetInsertBlock();
        irb.CreateBr(endBB);

        // 合并结果
        irb.SetInsertPoint(endBB);
        auto phi = irb.CreatePHI(llvm::Type::getInt1Ty(mCtx), 2);
        phi->addIncoming(llvm::ConstantInt::getFalse(mCtx), lhsBB);
        phi->addIncoming(rhsCond, rhsEndBB);

        // C语言要求逻辑操作结果为整型
        return irb.CreateZExt(phi, llvm::Type::getInt32Ty(mCtx));
    }

    // 特殊处理逻辑或运算，实现短路求值
    if (obj->op == BinaryExpr::kOr) {
        auto func = irb.GetInsertBlock()->getParent();
        auto lhsBB = irb.GetInsertBlock();
        auto rhsBB = llvm::BasicBlock::Create(mCtx, "lor.rhs", func);
        auto endBB = llvm::BasicBlock::Create(mCtx, "lor.end", func);

        // 计算左操作数
        auto lhsVal = (*this)(obj->lft);
        llvm::Value* lhsCond = lhsVal;
        
        // 确保比较的是布尔值
        if (!lhsCond->getType()->isIntegerTy(1)) {
            lhsCond = irb.CreateICmpNE(lhsCond, 
                llvm::ConstantInt::get(lhsCond->getType(), 0));
        }

        // 创建条件分支：如果左操作数为真，则短路到结果为真；否则，计算右操作数
        irb.CreateCondBr(lhsCond, endBB, rhsBB);

        // 计算右操作数
        irb.SetInsertPoint(rhsBB);
        auto rhsVal = (*this)(obj->rht);
        llvm::Value* rhsCond = rhsVal;
        
        // 确保比较的是布尔值
        if (!rhsCond->getType()->isIntegerTy(1)) {
            rhsCond = irb.CreateICmpNE(rhsCond, 
                llvm::ConstantInt::get(rhsCond->getType(), 0));
        }
        
        auto rhsEndBB = irb.GetInsertBlock();
        irb.CreateBr(endBB);

        // 合并结果
        irb.SetInsertPoint(endBB);
        auto phi = irb.CreatePHI(llvm::Type::getInt1Ty(mCtx), 2);
        phi->addIncoming(llvm::ConstantInt::getTrue(mCtx), lhsBB);
        phi->addIncoming(rhsCond, rhsEndBB);

        // C语言要求逻辑操作结果为整型
        return irb.CreateZExt(phi, llvm::Type::getInt32Ty(mCtx));
    }

    // 特殊处理数组索引运算
    if (obj->op == BinaryExpr::kIndex) {
        auto array = (*this)(obj->lft);
        auto index = (*this)(obj->rht);
        
        // 确保索引是32位整数
        if (!index->getType()->isIntegerTy(32)) {
            index = irb.CreateIntCast(index, llvm::Type::getInt32Ty(mCtx), true);
        }
        
        // 处理数组类型
        if (array->getType()->isArrayTy()) {
            // 数组作为值类型，需要使用GEP指令访问元素
            std::vector<llvm::Value*> indices = {
                llvm::ConstantInt::get(llvm::Type::getInt32Ty(mCtx), 0),
                index
            };
            return irb.CreateInBoundsGEP(array->getType(), array, indices);
        } 
        // 处理指针类型
        else if (array->getType()->isPointerTy()) {
            // 确定元素类型
            llvm::Type* elemType = nullptr;
            
            // 尝试从AST类型推断元素类型
            if (obj->lft->type && obj->lft->type->texp) {
                if (auto arrType = obj->lft->type->texp->dcst<ArrayType>()) {
                    Type elemAstType;
                    elemAstType.spec = obj->lft->type->spec;
                    elemAstType.qual = obj->lft->type->qual;
                    elemAstType.texp = arrType->sub;
                    elemType = (*this)(&elemAstType);
                } else if (auto ptrType = obj->lft->type->texp->dcst<PointerType>()) {
                    Type elemAstType;
                    elemAstType.spec = obj->lft->type->spec;
                    elemAstType.qual = obj->lft->type->qual;
                    elemAstType.texp = ptrType->sub;
                    elemType = (*this)(&elemAstType);
                }
            }
            
            // 默认使用int类型作为元素类型
            if (!elemType) {
                elemType = llvm::Type::getInt32Ty(mCtx);
            }
            
            // 使用GEP访问指针元素
            return irb.CreateInBoundsGEP(elemType, array, index);
        }
        
        std::cerr << "Error: Array indexing requires array or pointer type" << std::endl;
        ABORT();
    }

    // 处理普通二元运算符
    auto lhs = (*this)(obj->lft);
    auto rhs = (*this)(obj->rht);

    // 类型统一化：确保左右操作数类型一致
    if (lhs->getType() != rhs->getType()) {
        if (lhs->getType()->isIntegerTy() && rhs->getType()->isIntegerTy()) {
            auto lhsBits = lhs->getType()->getIntegerBitWidth();
            auto rhsBits = rhs->getType()->getIntegerBitWidth();
            
            // 提升至更大的整数类型
            if (lhsBits > rhsBits) {
                rhs = irb.CreateSExt(rhs, lhs->getType());
            } else {
                lhs = irb.CreateSExt(lhs, rhs->getType());
            }
        }
    }

    // 根据操作符类型生成相应指令
    switch (obj->op) {
        // 算术运算符
        case BinaryExpr::kAdd:
            return irb.CreateAdd(lhs, rhs);
        case BinaryExpr::kSub:
            return irb.CreateSub(lhs, rhs);
        case BinaryExpr::kMul:
            return irb.CreateMul(lhs, rhs);
        case BinaryExpr::kDiv:
            return irb.CreateSDiv(lhs, rhs);
        case BinaryExpr::kMod:
            return irb.CreateSRem(lhs, rhs);
        
        // 比较运算符
        case BinaryExpr::kGt:
            // C语言中比较操作返回整型，不是布尔值
            return irb.CreateZExt(irb.CreateICmpSGT(lhs, rhs), llvm::Type::getInt32Ty(mCtx));
        case BinaryExpr::kLt:
            return irb.CreateZExt(irb.CreateICmpSLT(lhs, rhs), llvm::Type::getInt32Ty(mCtx));
        case BinaryExpr::kGe:
            return irb.CreateZExt(irb.CreateICmpSGE(lhs, rhs), llvm::Type::getInt32Ty(mCtx));
        case BinaryExpr::kLe:
            return irb.CreateZExt(irb.CreateICmpSLE(lhs, rhs), llvm::Type::getInt32Ty(mCtx));
        case BinaryExpr::kEq:
            return irb.CreateZExt(irb.CreateICmpEQ(lhs, rhs), llvm::Type::getInt32Ty(mCtx));
        case BinaryExpr::kNe:
            return irb.CreateZExt(irb.CreateICmpNE(lhs, rhs), llvm::Type::getInt32Ty(mCtx));
            
        default:
            std::cerr << "Error: Unhandled BinaryExpr op: " << static_cast<int>(obj->op) << std::endl;
            ABORT();
    }
}

llvm::Value* EmitIR::operator()(CallExpr* obj) {
    auto& irb = *mCurIrb;
    
    // 获取函数指针
    auto func = (*this)(obj->head);
    
    // 获取参数列表
    std::vector<llvm::Value*> args;
    for (auto& arg : obj->args) {
        args.push_back((*this)(arg));
    }
    
    // 直接处理函数调用
    if (auto function = llvm::dyn_cast<llvm::Function>(func)) {
        return irb.CreateCall(function, args);
    }
    
    // 处理函数指针调用
    llvm::FunctionType* fnType = nullptr;
    
    // 尝试从AST类型获取函数类型
    if (obj->head->type && obj->head->type->texp) {
        auto funcType = (*this)(obj->head->type);
        if (funcType->isFunctionTy()) {
            fnType = llvm::cast<llvm::FunctionType>(funcType);
        }
    }
    
    // 如果无法获取函数类型，构建一个
    if (!fnType) {
        std::vector<llvm::Type*> argTypes;
        for (auto& arg : obj->args) {
            argTypes.push_back((*this)(arg->type));
        }
        fnType = llvm::FunctionType::get((*this)(obj->type), argTypes, false);
    }
    
    // 创建函数调用
    return irb.CreateCall(fnType, func, args);
}

void EmitIR::initializeArray(llvm::Value* arrayPtr, llvm::ArrayType* arrayType, InitListExpr* init) {
    auto& irb = *mCurIrb;
    
    // 检查是否是多维数组
    bool isNestedArray = arrayType->getElementType()->isArrayTy();
    unsigned numElements = arrayType->getNumElements();
    
    // 处理提供的初始化元素
    for (size_t i = 0; i < numElements; ++i) {
        // 创建GEP索引访问当前元素
        std::vector<llvm::Value*> indices = {
            llvm::ConstantInt::get(llvm::Type::getInt32Ty(mCtx), 0),
            llvm::ConstantInt::get(llvm::Type::getInt32Ty(mCtx), i)
        };
        
        // 获取当前元素的指针
        auto elemPtr = irb.CreateInBoundsGEP(arrayType, arrayPtr, indices);
        
        // 多维数组需要递归处理
        if (isNestedArray) {
            auto subArrayType = llvm::cast<llvm::ArrayType>(arrayType->getElementType());
            
            // 如果有对应的初始化表达式，继续递归初始化
            if (i < init->list.size()) {
                if (auto subInit = init->list[i]->dcst<InitListExpr>()) {
                    initializeArray(elemPtr, subArrayType, subInit);
                } else {
                    // 如果不是初始化列表但期望是数组，创建零初始化
                    auto elemVal = llvm::Constant::getNullValue(arrayType->getElementType());
                    irb.CreateStore(elemVal, elemPtr);
                }
            } else {
                // 超出init列表范围，使用零初始化
                auto elemVal = llvm::Constant::getNullValue(arrayType->getElementType());
                irb.CreateStore(elemVal, elemPtr);
            }
        } 
        // 基本类型直接存储
        else {
            if (i < init->list.size()) {
                auto elemVal = (*this)(init->list[i]);
                elemVal = ensureType(elemVal, arrayType->getElementType());
                irb.CreateStore(elemVal, elemPtr);
            } else {
                // 超出init列表范围，使用零初始化
                auto zero = llvm::Constant::getNullValue(arrayType->getElementType());
                irb.CreateStore(zero, elemPtr);
            }
        }
    }
}

llvm::Value* EmitIR::operator()(InitListExpr* obj) {
    auto& irb = *mCurIrb;
    
    // 获取目标类型
    llvm::Type* type = (*this)(obj->type);
    
    // 处理数组初始化
    if (auto arrayType = llvm::dyn_cast<llvm::ArrayType>(type)) {
        // 创建局部数组
        auto alloca = irb.CreateAlloca(arrayType);
        
        // 递归初始化数组元素
        initializeArray(alloca, arrayType, obj);
        
        return alloca;
    }
    
    // 处理标量类型初始化
    if (!obj->list.empty()) {
        return (*this)(obj->list[0]);
    }
    
    // 默认零初始化
    return llvm::Constant::getNullValue(type);
}

llvm::Value* EmitIR::operator()(ImplicitCastExpr* obj) {
    auto& irb = *mCurIrb;
    auto val = (*this)(obj->sub);
    
    switch (obj->kind) {
        case ImplicitCastExpr::kLValueToRValue:
            // 从内存加载值
            return irb.CreateLoad((*this)(obj->type), val);
            
        case ImplicitCastExpr::kIntegralCast:
            // 整数类型转换
            return ensureType(val, (*this)(obj->type));
            
        case ImplicitCastExpr::kArrayToPointerDecay:
            // 数组退化为指针
            if (val->getType()->isArrayTy()) {
                // 获取数组第一个元素的地址
                std::vector<llvm::Value*> indices = {
                    llvm::ConstantInt::get(llvm::Type::getInt32Ty(mCtx), 0),
                    llvm::ConstantInt::get(llvm::Type::getInt32Ty(mCtx), 0)
                };
                return irb.CreateInBoundsGEP(val->getType(), val, indices);
            }
            
            // 已经是指针，不需要处理
            if (val->getType()->isPointerTy()) {
                return val;
            }
            
            return val;
            
        case ImplicitCastExpr::kFunctionToPointerDecay:
        case ImplicitCastExpr::kNoOp:
            // 这些类型在LLVM中通常不需要额外处理
            return val;
            
        default:
            std::cerr << "Error: Unknown implicit cast kind: " << obj->kind << std::endl;
            ABORT();
    }
}

//==============================================================================
// 语句处理
//==============================================================================

void EmitIR::operator()(Stmt* obj) {
    if (!obj) {
        std::cerr << "Error: Null statement encountered" << std::endl;
        ABORT();
    }
    
    // 分发到具体类型的处理函数
    if (auto p = obj->dcst<CompoundStmt>())
        (*this)(p);
    else if (auto p = obj->dcst<ReturnStmt>())
        (*this)(p);
    else if (auto p = obj->dcst<DeclStmt>())
        (*this)(p);
    else if (auto p = obj->dcst<ExprStmt>())
        (*this)(p);
    else if (auto p = obj->dcst<IfStmt>())
        (*this)(p);
    else if (auto p = obj->dcst<WhileStmt>())
        (*this)(p);
    else if (auto p = obj->dcst<NullStmt>())
        (*this)(p);
    else if (auto p = obj->dcst<BreakStmt>())
        (*this)(p);
    else if (auto p = obj->dcst<ContinueStmt>())
        (*this)(p);
    else {
        std::cerr << "Error: Unknown statement type: " << typeid(*obj).name() << std::endl;
        ABORT();
    }
}

void EmitIR::operator()(NullStmt* obj) {
    // 空语句，不生成代码
}

void EmitIR::operator()(DeclStmt* obj) {
    // 处理声明语句中的所有声明
    for (auto&& decl : obj->decls) {
        (*this)(decl);
    }
}

void EmitIR::operator()(ExprStmt* obj) {
    // 处理表达式语句，计算表达式的值但不使用
    if (obj->expr) {
        (*this)(obj->expr);
    }
}

void EmitIR::operator()(CompoundStmt* obj) {
    // 处理复合语句，依次执行每个子语句
    for (auto&& stmt : obj->subs) {
        (*this)(stmt);
    }
}

void EmitIR::operator()(ReturnStmt* obj) {
    auto& irb = *mCurIrb;
    
    if (obj->expr) {
        // 处理有返回值的情况
        auto retVal = (*this)(obj->expr);
        
        // 确保返回值类型与函数返回类型一致
        auto retType = mCurFunc->getReturnType();
        retVal = ensureType(retVal, retType);
        
        irb.CreateRet(retVal);
    } else {
        // 处理无返回值的情况
        irb.CreateRetVoid();
    }
    
    // 创建不可达的基本块，用于后续代码
    // （return语句后的代码在该函数中不会被执行）
    auto exitBB = llvm::BasicBlock::Create(mCtx, "return_exit", mCurFunc);
    irb.SetInsertPoint(exitBB);
}

void EmitIR::operator()(IfStmt* obj) {
    auto& irb = *mCurIrb;
    auto func = irb.GetInsertBlock()->getParent();
    
    // 创建条件分支所需的基本块
    auto thenBB = llvm::BasicBlock::Create(mCtx, "if.then", func);
    auto endBB = llvm::BasicBlock::Create(mCtx, "if.end", func);
    auto elseBB = obj->else_ ? llvm::BasicBlock::Create(mCtx, "if.else", func) : endBB;
    
    // 生成条件判断代码，使用辅助函数处理短路求值
    emitCondBr(obj->cond, thenBB, elseBB);

    // 生成then分支代码
    irb.SetInsertPoint(thenBB);
    (*this)(obj->then);
    
    // 添加到end的跳转（如果没有终止指令）
    if (!irb.GetInsertBlock()->getTerminator()) {
        irb.CreateBr(endBB);
    }
    
    // 生成else分支代码（如果有）
    if (obj->else_) {
        irb.SetInsertPoint(elseBB);
        (*this)(obj->else_);
        
        // 添加到end的跳转（如果没有终止指令）
        if (!irb.GetInsertBlock()->getTerminator()) {
            irb.CreateBr(endBB);
        }
    }
    
    // 设置后续代码的插入点
    irb.SetInsertPoint(endBB);
}

void EmitIR::operator()(WhileStmt* obj) {
    auto& irb = *mCurIrb;
    auto func = irb.GetInsertBlock()->getParent();
    
    // 创建循环所需的基本块
    auto condBB = llvm::BasicBlock::Create(mCtx, "while.cond", func);
    auto bodyBB = llvm::BasicBlock::Create(mCtx, "while.body", func);
    auto endBB = llvm::BasicBlock::Create(mCtx, "while.end", func);
    
    // 保存循环信息，用于break和continue语句
    LoopInfo loopInfo = {condBB, bodyBB, endBB};
    mLoopStack.push(loopInfo);
    
    // 从当前基本块跳转到条件基本块
    irb.CreateBr(condBB);
    
    // 生成条件判断代码
    irb.SetInsertPoint(condBB);
    emitCondBr(obj->cond, bodyBB, endBB);
    
    // 生成循环体代码
    irb.SetInsertPoint(bodyBB);
    (*this)(obj->body);
    
    // 添加循环回边（如果没有终止指令）
    if (!irb.GetInsertBlock()->getTerminator()) {
        irb.CreateBr(condBB);
    }
    
    // 设置后续代码的插入点
    irb.SetInsertPoint(endBB);
    
    // 移除当前循环信息
    mLoopStack.pop();
}

void EmitIR::operator()(BreakStmt* obj) {
    if (mLoopStack.empty()) {
        std::cerr << "Error: Break statement outside of loop" << std::endl;
        ABORT();
    }
    
    auto& irb = *mCurIrb;
    
    // 跳转到当前循环的结束基本块
    irb.CreateBr(mLoopStack.top().endBlock);
    
    // 创建不可达的基本块，用于后续代码
    auto unreachableBB = llvm::BasicBlock::Create(mCtx, "unreachable", mCurFunc);
    irb.SetInsertPoint(unreachableBB);
}

void EmitIR::operator()(ContinueStmt* obj) {
    if (mLoopStack.empty()) {
        std::cerr << "Error: Continue statement outside of loop" << std::endl;
        ABORT();
    }
    
    auto& irb = *mCurIrb;
    
    // 跳转到当前循环的条件判断基本块
    irb.CreateBr(mLoopStack.top().condBlock);
    
    // 创建不可达的基本块，用于后续代码
    auto unreachableBB = llvm::BasicBlock::Create(mCtx, "unreachable", mCurFunc);
    irb.SetInsertPoint(unreachableBB);
}

//==============================================================================
// 声明处理
//==============================================================================

void EmitIR::operator()(Decl* obj) {
    // 分发到具体类型的处理函数
    if (auto p = obj->dcst<VarDecl>())
        (*this)(p);
    else if (auto p = obj->dcst<FunctionDecl>())
        (*this)(p);
    else if (auto p = obj->dcst<TranslationUnit>()) {
        for (auto&& i : p->decls)
            (*this)(i);
    }
    else {
        std::cerr << "Error: Unknown declaration type: " << typeid(*obj).name() << std::endl;
        ABORT();
    }
}

void EmitIR::operator()(VarDecl* obj) {
    auto& irb = *mCurIrb;
    llvm::Type* varType = (*this)(obj->type);
    
    // 全局变量处理
    if (mCurFunc == nullptr) {
        // 处理数组类型
        if (varType->isArrayTy()) {
            // 创建全局数组变量
            llvm::GlobalVariable* globalVar = new llvm::GlobalVariable(
                mMod, 
                varType, 
                obj->type->qual.const_, 
                llvm::GlobalVariable::ExternalLinkage,
                llvm::Constant::getNullValue(varType), // 确保零初始化
                obj->name
            );
            
            // 如果有初始化表达式
            if (obj->init) {
                if (auto initList = obj->init->dcst<InitListExpr>()) {
                    // 对于常量初始化
                    if (obj->type->qual.const_) {
                        // 尝试创建常量初始化器
                        auto arrayType = llvm::cast<llvm::ArrayType>(varType);
                        std::vector<llvm::Constant*> constValues;
                        
                        // 填充指定的初始值
                        for (size_t i = 0; i < initList->list.size() && i < arrayType->getNumElements(); ++i) {
                            if (auto intLit = initList->list[i]->dcst<IntegerLiteral>()) {
                                constValues.push_back(llvm::ConstantInt::get(arrayType->getElementType(), intLit->val));
                            } else {
                                constValues.push_back(llvm::Constant::getNullValue(arrayType->getElementType()));
                            }
                        }
                        
                        // 确保所有元素都被初始化
                        while (constValues.size() < arrayType->getNumElements()) {
                            constValues.push_back(llvm::Constant::getNullValue(arrayType->getElementType()));
                        }
                        
                        // 设置常量初始化器
                        auto initializer = llvm::ConstantArray::get(arrayType, constValues);
                        globalVar->setInitializer(initializer);
                    } 
                    else {
                        // 对于非常量初始化，使用运行时构造函数
                        createGlobalCtor(obj->name, obj->init, globalVar);
                    }
                } else {
                    // 非列表初始化
                    auto initVal = (*this)(obj->init);
                    // 检查是否可以作为常量
                    if (auto constVal = llvm::dyn_cast<llvm::Constant>(initVal)) {
                        globalVar->setInitializer(constVal);
                    } else {
                        globalVar->setInitializer(llvm::Constant::getNullValue(varType));
                        createGlobalCtor(obj->name, obj->init, globalVar);
                    }
                }
            }
            
            obj->any = static_cast<void*>(globalVar);
            return;
        }
        
        // 处理非数组全局变量
        llvm::GlobalVariable* globalVar = new llvm::GlobalVariable(
            mMod, 
            varType, 
            obj->type->qual.const_, // isConstant
            llvm::GlobalVariable::ExternalLinkage, 
            nullptr, // 先不设置初始值
            obj->name
        );
        
        // 设置初始化值
        if (obj->init) {
            if (auto literal = obj->init->dcst<IntegerLiteral>()) {
                // 直接用常量初始化
                globalVar->setInitializer(llvm::ConstantInt::get((*this)(obj->type), literal->val));
            } else {
                // 使用零初始化并创建构造函数
                globalVar->setInitializer(llvm::Constant::getNullValue(varType));
                createGlobalCtor(obj->name, obj->init, globalVar);
            }
        } else {
            // 无初始值，使用零初始化
            globalVar->setInitializer(llvm::Constant::getNullValue(varType));
        }
        
        obj->any = static_cast<void*>(globalVar);
    }
    // 局部变量处理
    else {
        // 避免重复处理
        if (obj->any) return;
        
        // 为局部变量分配栈空间
        llvm::AllocaInst* alloca = irb.CreateAlloca(varType, nullptr, obj->name);
        
        // 处理初始化
        if (obj->init) {
            // 特殊处理数组初始化
            if (varType->isArrayTy() && obj->init->dcst<InitListExpr>()) {
                auto initList = obj->init->dcst<InitListExpr>();
                auto arrayType = llvm::cast<llvm::ArrayType>(varType);
                initializeArray(alloca, arrayType, initList);
            } else {
                // 非数组类型直接初始化
                auto initVal = (*this)(obj->init);
                initVal = ensureType(initVal, varType);
                irb.CreateStore(initVal, alloca);
            }
        }
        
        obj->any = static_cast<void*>(alloca);
    }
}

void EmitIR::operator()(FunctionDecl* obj) {
    // 创建函数
    auto fty = llvm::dyn_cast<llvm::FunctionType>((*this)(obj->type));
    auto func = llvm::Function::Create(
        fty, 
        llvm::GlobalVariable::ExternalLinkage, 
        obj->name, 
        mMod
    );
    
    // 保存函数引用
    obj->any = func;
    
    // 只处理有函数体的函数
    if (obj->body == nullptr)
        return;
        
    // 创建函数入口基本块
    auto entryBB = llvm::BasicBlock::Create(mCtx, "entry", func);
    mCurIrb->SetInsertPoint(entryBB);
    auto& entryIrb = *mCurIrb;
    
    // 保存当前处理的函数
    auto prevFunc = mCurFunc;
    mCurFunc = func;
    
    // 处理函数参数
    unsigned i = 0;
    for (auto iter = func->arg_begin(); iter != func->arg_end() && i < obj->params.size(); ++iter, ++i) {
        auto param = obj->params[i];
        iter->setName(param->name);
        
        // 在栈上分配空间存储参数
        llvm::AllocaInst* alloca = entryIrb.CreateAlloca((*this)(param->type), nullptr, param->name);
        entryIrb.CreateStore(&*iter, alloca);
        
        // 保存参数地址
        param->any = static_cast<void*>(alloca);
    }
    
    // 翻译函数体
    (*this)(obj->body);
    
    // 确保函数有返回值
    auto& exitIrb = *mCurIrb;
    if (!exitIrb.GetInsertBlock()->getTerminator()) {
        if (fty->getReturnType()->isVoidTy()) {
            exitIrb.CreateRetVoid();
        } else {
            // 非void函数必须有返回值，返回默认值0
            exitIrb.CreateRet(llvm::ConstantInt::get(fty->getReturnType(), 0));
        }
    }
    
    // 恢复之前的函数上下文
    mCurFunc = prevFunc;
}
