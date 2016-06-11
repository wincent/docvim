module Text.Docvim.Optimize (optimize) where

import Control.Lens hiding (Empty)
import Text.Docvim.AST

-- | "Optimize" a Project's AST by eliminating empty paths.
optimize :: Node -> Node
optimize = transform prune

-- | Marks a node for pruning by returning Empty if it has no non-Empty
-- children.
prune :: Node -> Node
prune n | not (null (children n)) = if all empty (children n) then Empty else n
        | container n = Empty
        | otherwise = n

-- | Returns True if the supplied node is the Empty node.
empty :: Node -> Bool
empty Empty = True
empty _     = False

-- | Returns True if the supplied node is a (potentially) prunable container.
container :: Node -> Bool
container (Project _)  = True
container (Unit _)     = True
container (DocBlock _) = True
container _            = False
