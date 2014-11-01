module SeqQueue (Queue, empty, enqueue, dequeue, isEmpty) where

import qualified Data.Sequence as Seq
import AbstractQueue

newtype Queue t = QueueImpl (Seq.Seq t)

instance AbstractQueue Queue where
  empty = QueueImpl Seq.empty

  isEmpty (QueueImpl s) = Seq.null s

  enqueue (QueueImpl s) x = QueueImpl (x Seq.<| s)

  dequeue (QueueImpl s) = (x, QueueImpl q)
    where
      (q Seq.:> x) = Seq.viewr s
