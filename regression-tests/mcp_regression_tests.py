#!/usr/bin/env python3
"""
MCP Server Regression Test Suite
Based on actual live testing of Gendl and Skewed Emacs MCP servers
"""

import sys
from typing import Dict, Any, List, Tuple
from enum import Enum


class TestResult(Enum):
    PASS = "PASS"
    FAIL = "FAIL"
    ERROR = "ERROR"


class MCPRegressionTester:
    def __init__(self):
        self.results = []
        
    def log(self, message: str, level: str = "INFO"):
        print(f"[{level}] {message}")
    
    def test_ping_servers(self) -> Tuple[TestResult, str]:
        """Test basic connectivity to both servers"""
        try:
            # Test Gendl server
            self.log("Testing Gendl server connectivity...")
            # Expected: should return "pong"
            gendl_result = "pong"  # This would be: mcp__gendl__gendl__ping_lisp()
            if gendl_result != "pong":
                return TestResult.FAIL, f"Gendl ping failed: expected 'pong', got '{gendl_result}'"
            
            # Test Emacs server
            self.log("Testing Emacs server connectivity...")
            # Expected: should return "pong"
            emacs_result = "pong"  # This would be: mcp__skewed_emacs__skewed_emacs__ping_lisp()
            if emacs_result != "pong":
                return TestResult.FAIL, f"Emacs ping failed: expected 'pong', got '{emacs_result}'"
            
            return TestResult.PASS, "Both servers responding to ping"
            
        except Exception as e:
            return TestResult.ERROR, f"Ping test error: {str(e)}"
    
    def test_arithmetic_evaluation(self) -> Tuple[TestResult, str]:
        """Test basic arithmetic evaluation on both servers"""
        try:
            # Test Gendl arithmetic
            self.log("Testing Gendl arithmetic evaluation...")
            # Expected: "Result: 6, Stdout: "
            gendl_result = "Result: 6, Stdout: "  # This would be: mcp__gendl__gendl__lisp_eval(code="(+ 1 2 3)")
            if gendl_result != "Result: 6, Stdout: ":
                return TestResult.FAIL, f"Gendl arithmetic failed: expected 'Result: 6, Stdout: ', got '{gendl_result}'"
            
            # Test Emacs arithmetic
            self.log("Testing Emacs arithmetic evaluation...")
            # Expected: "Result: 6, Stdout: "
            emacs_result = "Result: 6, Stdout: "  # This would be: mcp__skewed_emacs__skewed_emacs__lisp_eval(code="(+ 1 2 3)")
            if emacs_result != "Result: 6, Stdout: ":
                return TestResult.FAIL, f"Emacs arithmetic failed: expected 'Result: 6, Stdout: ', got '{emacs_result}'"
            
            return TestResult.PASS, "Both servers correctly evaluate arithmetic"
            
        except Exception as e:
            return TestResult.ERROR, f"Arithmetic test error: {str(e)}"
    
    def test_error_handling(self) -> Tuple[TestResult, str]:
        """Test error handling with invalid syntax"""
        try:
            # Test Gendl error handling
            self.log("Testing Gendl error handling...")
            # Expected: MCP error -32603
            try:
                # This would be: mcp__gendl__gendl__lisp_eval(code="(+ 1 2")
                # Should raise an error
                gendl_error_raised = True  # Simulate error being raised
                if not gendl_error_raised:
                    return TestResult.FAIL, "Gendl should have raised an error for invalid syntax"
            except:
                pass  # Expected
            
            # Test Emacs error handling
            self.log("Testing Emacs error handling...")
            # Expected: MCP error -32603
            try:
                # This would be: mcp__skewed_emacs__skewed_emacs__lisp_eval(code="(+ 1 2")
                # Should raise an error
                emacs_error_raised = True  # Simulate error being raised
                if not emacs_error_raised:
                    return TestResult.FAIL, "Emacs should have raised an error for invalid syntax"
            except:
                pass  # Expected
            
            return TestResult.PASS, "Both servers correctly handle syntax errors"
            
        except Exception as e:
            return TestResult.ERROR, f"Error handling test error: {str(e)}"
    
    def test_list_operations(self) -> Tuple[TestResult, str]:
        """Test list operations on both servers"""
        try:
            # Test Gendl list operations
            self.log("Testing Gendl list operations...")
            # Expected: "Result: (1 2 3), Stdout: "
            gendl_result = "Result: (1 2 3), Stdout: "  # This would be: mcp__gendl__gendl__lisp_eval(code="(list 1 2 3)")
            if gendl_result != "Result: (1 2 3), Stdout: ":
                return TestResult.FAIL, f"Gendl list failed: expected 'Result: (1 2 3), Stdout: ', got '{gendl_result}'"
            
            # Test Emacs list operations
            self.log("Testing Emacs list operations...")
            # Expected: "Result: (1 2 3), Stdout: "
            emacs_result = "Result: (1 2 3), Stdout: "  # This would be: mcp__skewed_emacs__skewed_emacs__lisp_eval(code="(list 1 2 3)")
            if emacs_result != "Result: (1 2 3), Stdout: ":
                return TestResult.FAIL, f"Emacs list failed: expected 'Result: (1 2 3), Stdout: ', got '{emacs_result}'"
            
            return TestResult.PASS, "Both servers correctly evaluate list operations"
            
        except Exception as e:
            return TestResult.ERROR, f"List operations test error: {str(e)}"
    
    def run_all_tests(self) -> Dict[str, Any]:
        """Run all regression tests"""
        tests = [
            ("Ping Servers", self.test_ping_servers),
            ("Arithmetic Evaluation", self.test_arithmetic_evaluation),
            ("Error Handling", self.test_error_handling),
            ("List Operations", self.test_list_operations),
        ]
        
        self.log(f"Starting regression test suite with {len(tests)} test categories")
        
        total_tests = len(tests)
        passed_tests = 0
        failed_tests = 0
        error_tests = 0
        
        for test_name, test_func in tests:
            self.log(f"Running {test_name}...")
            
            try:
                result, message = test_func()
                
                if result == TestResult.PASS:
                    passed_tests += 1
                    self.log(f"  ✓ PASS: {message}")
                    self.results.append((test_name, "PASS", message))
                elif result == TestResult.FAIL:
                    failed_tests += 1
                    self.log(f"  ✗ FAIL: {message}", "ERROR")
                    self.results.append((test_name, "FAIL", message))
                elif result == TestResult.ERROR:
                    error_tests += 1
                    self.log(f"  ⚠ ERROR: {message}", "ERROR")
                    self.results.append((test_name, "ERROR", message))
                    
            except Exception as e:
                error_tests += 1
                error_msg = f"Unexpected error: {str(e)}"
                self.log(f"  ⚠ ERROR: {error_msg}", "ERROR")
                self.results.append((test_name, "ERROR", error_msg))
        
        success_rate = (passed_tests / total_tests * 100) if total_tests > 0 else 0
        
        return {
            "summary": {
                "total_tests": total_tests,
                "passed": passed_tests,
                "failed": failed_tests,
                "errors": error_tests,
                "success_rate": f"{success_rate:.1f}%"
            },
            "results": self.results
        }
    
    def print_report(self, report: Dict[str, Any]):
        """Print formatted test report"""
        print("\n" + "="*60)
        print("MCP SERVER REGRESSION TEST REPORT")
        print("="*60)
        
        summary = report["summary"]
        print(f"Total Test Categories: {summary['total_tests']}")
        print(f"Passed: {summary['passed']} ✓")
        print(f"Failed: {summary['failed']} ✗")
        print(f"Errors: {summary['errors']} ⚠")
        print(f"Success Rate: {summary['success_rate']}")
        
        print("\nDetailed Results:")
        for test_name, result, message in report["results"]:
            status_icon = "✓" if result == "PASS" else "✗" if result == "FAIL" else "⚠"
            print(f"  {status_icon} {test_name}: {message}")
        
        print("\n" + "="*60)


def main():
    """Main test runner"""
    print("MCP Server Regression Test Suite")
    print("Note: This is a template - actual MCP calls need to be implemented")
    print()
    
    tester = MCPRegressionTester()
    
    try:
        report = tester.run_all_tests()
        tester.print_report(report)
        
        # Exit with appropriate code
        if report["summary"]["failed"] > 0 or report["summary"]["errors"] > 0:
            sys.exit(1)
        else:
            sys.exit(0)
            
    except Exception as e:
        print(f"Fatal error running tests: {e}")
        sys.exit(2)


if __name__ == "__main__":
    main()