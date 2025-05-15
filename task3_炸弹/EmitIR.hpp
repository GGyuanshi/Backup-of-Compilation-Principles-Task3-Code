#pragma once

#include "asg.hpp"
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <map>
#include <stack>
#include <functional>

class EmitIR
{
public:
  /**
   * @brief 构造IR生成器
   * @param mgr 对象管理器
   * @param ctx LLVM上下文
   * @param mid 模块标识名
   */
  EmitIR(Obj::Mgr& mgr, llvm::LLVMContext& ctx, llvm::StringRef mid = "-");
  
  /**
   * @brief 处理翻译单元，生成完整的LLVM模块
   * @param tu 要处理的翻译单元AST节点
   * @return 生成的LLVM模块引用
   */
  llvm::Module& operator()(asg::TranslationUnit* tu);

private:
  Obj::Mgr& mMgr;                   ///< 对象管理器引用
  llvm::Module mMod;                ///< 生成的LLVM模块
  llvm::LLVMContext& mCtx;          ///< LLVM上下文引用
  llvm::Type* mIntTy;               ///< 标准整型缓存
  llvm::FunctionType* mCtorTy;      ///< 构造函数类型缓存
  llvm::Function* mCurFunc = nullptr; ///< 当前处理的函数
  std::unique_ptr<llvm::IRBuilder<>> mCurIrb; ///< 当前IR构建器

  //==============================================================================
  // 辅助结构和类型定义
  //==============================================================================
  
  /**
   * @brief 循环控制流信息
   * 用于break和continue语句的目标基本块记录
   */
  struct LoopInfo {
    llvm::BasicBlock* condBlock;  ///< 循环条件检查的基本块
    llvm::BasicBlock* bodyBlock;  ///< 循环体的基本块
    llvm::BasicBlock* endBlock;   ///< 循环结束的基本块
  };
  std::stack<LoopInfo> mLoopStack;  ///< 循环嵌套栈
  
  //==============================================================================
  // 实用工具函数
  //==============================================================================
  
  /**
   * @brief 生成条件分支
   * @param cond 条件表达式
   * @param trueBB 条件为真时跳转的基本块
   * @param falseBB 条件为假时跳转的基本块
   * 
   * 针对逻辑运算符实现短路求值的特殊处理
   */
  void emitCondBr(asg::Expr* cond, llvm::BasicBlock* trueBB, llvm::BasicBlock* falseBB);
  
  /**
   * @brief 初始化数组
   * @param arrayPtr 数组指针
   * @param arrayType 数组类型
   * @param init 初始化表达式列表
   * 
   * 处理数组初始化，支持多维数组和零初始化
   */
  void initializeArray(llvm::Value* arrayPtr, llvm::ArrayType* arrayType, asg::InitListExpr* init);
  
  /**
   * @brief 创建全局变量构造函数
   * @param varName 变量名
   * @param initValue 初始化表达式
   * @param globalVar 全局变量指针
   * 
   * 为全局变量创建构造函数进行复杂初始化
   */
  void createGlobalCtor(const std::string& varName, 
                        asg::Expr* initExpr, 
                        llvm::GlobalVariable* globalVar);
  
  /**
   * @brief 确保类型匹配，执行必要的类型转换
   * @param value 要转换的值
   * @param targetType 目标类型
   * @return 转换后的值
   */
  llvm::Value* ensureType(llvm::Value* value, llvm::Type* targetType);

  //==============================================================================
  // 类型处理
  //==============================================================================

  /**
   * @brief 将ASG类型转换为LLVM类型
   */
  llvm::Type* operator()(const asg::Type* type);

  //==============================================================================
  // 表达式处理
  //==============================================================================

  /**
   * @brief 表达式分派处理函数
   */
  llvm::Value* operator()(asg::Expr* obj);
  
  /**
   * @brief 处理各种表达式类型
   */
  llvm::Value* operator()(asg::ImplicitInitExpr* obj);
  llvm::Constant* operator()(asg::IntegerLiteral* obj);
  llvm::Value* operator()(asg::StringLiteral* obj);
  llvm::Value* operator()(asg::DeclRefExpr* obj);
  llvm::Value* operator()(asg::ParenExpr* obj);
  llvm::Value* operator()(asg::UnaryExpr* obj);
  llvm::Value* operator()(asg::BinaryExpr* obj);
  llvm::Value* operator()(asg::CallExpr* obj);
  llvm::Value* operator()(asg::InitListExpr* obj);
  llvm::Value* operator()(asg::ImplicitCastExpr* obj);

  //==============================================================================
  // 语句处理
  //==============================================================================

  /**
   * @brief 语句分派处理函数
   */
  void operator()(asg::Stmt* obj);

  /**
   * @brief 处理各种语句类型
   */
  void operator()(asg::CompoundStmt* obj);
  void operator()(asg::ReturnStmt* obj);
  void operator()(asg::DeclStmt* obj);
  void operator()(asg::ExprStmt* obj);
  void operator()(asg::IfStmt* obj);
  void operator()(asg::WhileStmt* obj);
  void operator()(asg::NullStmt* obj);
  void operator()(asg::BreakStmt* obj);
  void operator()(asg::ContinueStmt* obj);

  //==============================================================================
  // 声明处理
  //==============================================================================

  /**
   * @brief 声明分派处理函数
   */
  void operator()(asg::Decl* obj);

  /**
   * @brief 处理各种声明类型
   */
  void operator()(asg::FunctionDecl* obj);
  void operator()(asg::VarDecl* obj);
};
