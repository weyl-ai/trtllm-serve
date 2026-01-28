{-# LANGUAGE OverloadedStrings #-}

-- | Tool Server Tests
module Main where

import Test.Hspec
import qualified CodeSandbox as CS
import qualified Data.Text as T

main :: IO ()
main = hspec $ do
  describe "CodeSandbox" $ do
    describe "detectLanguage" $ do
      it "detects Rust files" $ do
        CS.detectLanguage "main.rs" `shouldBe` Just CS.Rust
      
      it "detects Haskell files" $ do
        CS.detectLanguage "Main.hs" `shouldBe` Just CS.Haskell
        CS.detectLanguage "Main.lhs" `shouldBe` Just CS.Haskell
      
      it "detects Lean files" $ do
        CS.detectLanguage "Foo.lean" `shouldBe` Just CS.Lean
      
      it "detects Dhall files" $ do
        CS.detectLanguage "config.dhall" `shouldBe` Just CS.Dhall
      
      it "detects PureScript files" $ do
        CS.detectLanguage "Main.purs" `shouldBe` Just CS.PureScript
      
      it "returns Nothing for unknown extensions" $ do
        CS.detectLanguage "file.txt" `shouldBe` Nothing
        CS.detectLanguage "file.py" `shouldBe` Nothing
    
    describe "compileCommand" $ do
      it "generates rustc command for Rust" $ do
        let cmd = CS.compileCommand CS.Rust "/ws" "/ws/main.rs"
        CS.ccProgram cmd `shouldBe` "rustc"
        CS.ccWorkDir cmd `shouldBe` "/ws"
      
      it "generates ghc command for Haskell" $ do
        let cmd = CS.compileCommand CS.Haskell "/ws" "/ws/Main.hs"
        CS.ccProgram cmd `shouldBe` "ghc"
        "-fno-code" `elem` CS.ccArgs cmd `shouldBe` True
      
      it "generates lean command for Lean" $ do
        let cmd = CS.compileCommand CS.Lean "/ws" "/ws/Foo.lean"
        CS.ccProgram cmd `shouldBe` "lean"
      
      it "generates dhall command for Dhall" $ do
        let cmd = CS.compileCommand CS.Dhall "/ws" "/ws/config.dhall"
        CS.ccProgram cmd `shouldBe` "dhall"
        "type" `elem` CS.ccArgs cmd `shouldBe` True

  describe "Workspace operations" $ do
    it "creates and retrieves workspace" $ do
      state <- CS.newSandboxState
      ws <- CS.createWorkspace state (Just $ CS.WorkspaceId "test-ws")
      CS.unWorkspaceId (CS.wsId ws) `shouldBe` "test-ws"
      
      mWs <- CS.getWorkspace state (CS.WorkspaceId "test-ws")
      mWs `shouldBe` Just ws
    
    it "returns same workspace for duplicate id" $ do
      state <- CS.newSandboxState
      ws1 <- CS.createWorkspace state (Just $ CS.WorkspaceId "dup-test")
      ws2 <- CS.createWorkspace state (Just $ CS.WorkspaceId "dup-test")
      ws1 `shouldBe` ws2

  describe "File operations" $ do
    it "writes and reads files" $ do
      state <- CS.newSandboxState
      ws <- CS.createWorkspace state Nothing
      
      writeResult <- CS.writeFile ws "test.txt" "hello world"
      writeResult `shouldBe` Right ()
      
      readResult <- CS.readFile ws "test.txt"
      readResult `shouldBe` Right "hello world"
    
    it "handles nested directories" $ do
      state <- CS.newSandboxState
      ws <- CS.createWorkspace state Nothing
      
      writeResult <- CS.writeFile ws "src/main/Test.hs" "module Test where"
      writeResult `shouldBe` Right ()
      
      readResult <- CS.readFile ws "src/main/Test.hs"
      readResult `shouldBe` Right "module Test where"
    
    it "lists files recursively" $ do
      state <- CS.newSandboxState
      ws <- CS.createWorkspace state Nothing
      
      _ <- CS.writeFile ws "a.txt" "a"
      _ <- CS.writeFile ws "dir/b.txt" "b"
      _ <- CS.writeFile ws "dir/sub/c.txt" "c"
      
      files <- CS.listFiles ws
      length files `shouldBe` 3
      "a.txt" `elem` files `shouldBe` True
      "dir/b.txt" `elem` files `shouldBe` True
      "dir/sub/c.txt" `elem` files `shouldBe` True
