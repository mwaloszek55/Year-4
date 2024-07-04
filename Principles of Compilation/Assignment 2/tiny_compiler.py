import pickle


class TinyCompiler:

    def __init__(self, sourcepath):
        with open(sourcepath, "rb") as p2file:
            self.parse_tree = pickle.load(p2file)
        self.__varcount = 0
        self.__labcount = 0
        self.__tac = ""
    
    
    def __codegen(self, root):
        """ Generate TAC for construct represented by subtree 'root'. """
        actions = {
            "stmtseq": self.__codegen_stmtseq,
            "readstmt": self.__codegen_readstmt,
            "writestmt": self.__codegen_writestmt,
            "assignstmt": self.__codegen_assignstmt,
            "ifstmt": self.__codegen_ifstmt,
            "repeatstmt": self.__codegen_repeatstmt,
        }
        
        if root.label in actions:
            actions[root.label](root)
        else:
            if len(root.children) > 0:
                self.__codegen(root.children[0])


    def translate(self, output_filename):
        self.__varcount, self.__labcount = 0, 0
        
        print("Dumping the parse tree:")
        self.dump_tree(self.parse_tree)
        print("End of parse tree dump")
        
        self.__codegen(self.parse_tree)
        self.__tac += "halt;\n"
        with open(output_filename, 'w') as tac_file:
            tac_file.writelines(line for line in self.__tac)


    def dump_tree(self, node, indent=""):
        if hasattr(node, "children") and node.children:
            print(f"{indent}{node.label}")
            for child in node.children:
                self.dump_tree(child, indent + "  ")
        else:
            print(f"{indent}{node.label}: {node.value}")
    

    def __codegen_stmtseq(self, root):
        """ Generate TAC for statement list represented by subtree 
        'root'.
        """
        for c in root.children:
            self.__codegen(c)


    def __codegen_exp(self, root):
        """ Generate TAC for expression represented by subtree 'root'. """
        if len(root.children) == 1:
            return self.__codegen_simple_expr(root.children[0])

        exp_var = self.__new_var()
        simp_exp_var_1 = self.__codegen_simple_expr(root.children[0])

        comp_op = self.__codegen_leaf(root.children[1].children[0])
        comp_op = "==" if comp_op == "=" else comp_op

        simp_exp_var_2 = self.__codegen_simple_expr(root.children[2])
        self.__tac += "{} := {} {} {};\n".format(exp_var, simp_exp_var_1, comp_op, simp_exp_var_2)
        return exp_var
    

    def __codegen_simple_expr(self, root):
        """ Generate TAC for simple expression represented by subtree 'root'. """
        
        first_term_var = self.__codegen_term(root.children[0])
        if len(root.children) == 1:
            return first_term_var

        total_var = self.__new_var()
        self.__tac += "{} := {};\n".format(total_var, first_term_var)

        op = "+"
        index = 1
        while index < len(root.children):
            c = root.children[index]
            if c.label == "term":
                fvar = self.__codegen_term(c)
                self.__tac += "{} := {} {} {};\n".format(total_var, total_var, op, fvar)
            else:
                op = c.value  
                op = self.__codegen_leaf(c.children[0])
            index += 1

        return total_var
    

    def __codegen_term(self, root):
        """ Generate TAC for term represented by subtree 'root'. """
        first_factor_var = self.__codegen_leaf(root.children[0].children[0])
        if len(root.children) == 1:
            return first_factor_var

        total_var = self.__new_var()
        self.__tac += "{} := {};\n".format(total_var, first_factor_var)

        op = "*" 
        index = 1
        while index < len(root.children):
            c = root.children[index]
            if c.label == "factor":
                fvar = self.__codegen_leaf(c.children[0])
                self.__tac += "{} := {} {} {};\n".format(total_var, total_var, op, fvar)
            else:
                op = self.__codegen_leaf(c.children[0])
            index += 1

        return total_var


    def __codegen_readstmt(self, root):
        """ Generate TAC for read statement represented by subtree 'root'.
        """
        varname = root.children[0].value
        self.__tac += "{} := in;\n".format(varname)
    

    def __codegen_writestmt(self, root):
        """ Generate TAC for write statement represented by subtree 
        'root'.
        """
        exprvar = self.__codegen_exp(root.children[0])
        self.__tac += "out := {};\n".format(exprvar)


    def __codegen_leaf(self, root):
        """ Generate TAC for a leaf node. """
        fval = root.value
        if type(fval) == int:
            return str(fval)
        elif type(fval) == str:
            return fval
        else:
            return self.__codegen_exp(root.children[0])
        
        

    def __codegen_ifstmt(self, root):
        """ Generate TAC for if statement represented by subtree 
       'root'.
        """
        conditvar = self.__codegen_exp(root.children[0])

        skiptrue_label = self.__new_label()
        skipfalse_label = None

        self.__tac += "if ({} == 0) goto {}\n".format(conditvar, skiptrue_label)

        self.__codegen(root.children[1])

        if len(root.children) > 2:
            skipfalse_label = self.__new_label()
            self.__tac += "goto {}\n".format(skipfalse_label)

        self.__tac += "{}:\n".format(skiptrue_label)
        if skipfalse_label:
            self.__codegen(root.children[2])
            self.__tac += "{}:\n".format(skipfalse_label)



    def __codegen_assignstmt(self, root):
        """ Generate TAC for assignment statement represented by subtree 
        'root'.
        """
        destvar = self.__codegen_leaf(root.children[0])
        rhsvar = self.__codegen_exp(root.children[1])
        self.__tac += "{} := {};\n".format(destvar, rhsvar)

    

    def __codegen_repeatstmt(self, root):
        """ Generate TAC for while statement represented by subtree 'root'. """
        top_label = self.__new_label()
        self.__tac += "{}:\n".format(top_label)
        self.__codegen_stmtseq(root.children[0])
        conditvar = self.__codegen_exp(root.children[1])
        self.__tac += "if ({} == 0) goto {}\n".format(conditvar, top_label)
        bottom_label = self.__new_label()
        self.__tac += "{}:\n".format(bottom_label)



    def __new_var(self):
        """ Generate and return fresh temporray variable name.
        """
        self.__varcount += 1
        return "t%d" % self.__varcount
    
    def __new_label(self):
        """ Generate and return fresh label name.
        """
        self.__labcount += 1
        return "l%d" % self.__labcount






if __name__ == "__main__":
    compiler = TinyCompiler('onetoten_pt_node.pkl')
    compiler.translate('onetoten_pt_node.tac')
    print("\n\nTAC FILE:\n")
    with open('onetoten_pt_node.tac', 'r') as tac_file:
        print(tac_file.read())
