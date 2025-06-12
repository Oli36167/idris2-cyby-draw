module CyBy.Draw.Internal.Navigation

import Chem.Util
import CyBy.Draw.Draw
import CyBy.Draw.Event
import CyBy.Draw.Internal.Abbreviations
import CyBy.Draw.Internal.Atom
import CyBy.Draw.Internal.CoreDims
import CyBy.Draw.Internal.Graph
import CyBy.Draw.Internal.Label
import CyBy.Draw.Internal.Ring
import CyBy.Draw.Internal.Role
import CyBy.Draw.Internal.Settings
import CyBy.Draw.PeriodicTableCanvas
import Derive.Prelude
import Geom
import Text.Molfile
import Text.SVG
import Debug.Trace
import Data.Graph.Indexed.Util

%default total
%language ElabReflection
%hide Language.Reflection.TTImp.Mode

-- Returns a list of pairs with bond angles and the corresponding
-- 'global' Fin k of all visible neighbours.
bondAnglesWithNodes : CDIGraph k -> Fin k -> List (Angle, Fin k)
bondAnglesWithNodes g x =
  let p  := pointId (lab g x)
      ns := visibleNeighbours g x
  in mapMaybe (\fn => (,fn) <$> angle (pointId (lab g fn) - p)) ns

-- Returns the shortest distance between two angles. 
-- (either clockwise or counterclockwise.)
minDelta : Angle -> Angle -> Angle
minDelta x y = min (delta x y) (negate (delta x y))

-- Given an input angle and two nodes (with their positions and indices),
-- returns the Fin k of the node that lies in the direction most closely
-- matching the input angle.
bestPointId : Angle -> (Point Id, Fin k) -> (Point Id, Fin k) -> Fin k
bestPointId a (p1, i1) (p2, i2) =
    if      a == zero        then if p1.x >= p2.x then i1 else i2
    else if a == halfPi      then if p1.y >= p2.y then i1 else i2
    else if a == pi          then if p1.x <= p2.x then i1 else i2
    else if a == threeHalfPi then if p1.y <= p2.y then i1 else i2
    else i1

-- An angular direction margin that is slightly smaller than half pi, in order
-- to prevent the active node/edge to change perpendicular to the input angle.
DirectionMargin : Angle
DirectionMargin = Geom.Angle.angle (7 * pi / 16)

-- Returns the angle of the line connecting two nodes, if possible,
-- regardless of whether they are directly connected in the graph.
angleEdge : CDIGraph k -> Fin k -> Fin k -> Maybe Angle
angleEdge g n1 n2 =
  let p1 := pointId (lab g n1)
      p2 := pointId (lab g n2)
   in angle (p2 - p1)

-- Given and input Angle, it finds the best possible node/edge and changes
-- the role 'Hover' to that new node/edge. If no best node/edge is found,
-- the graph is returned unchanged.
export
newNode : {k : _} -> Angle -> CDIGraph k -> CDIGraph k
newNode a g =
  case hoveredItem g of
    N (i, _) =>
      case minBy (minDelta a) (bondAngles g i) of
        Just b =>
          if minDelta b a < DirectionMargin then
            case lookup b (bondAnglesWithNodes g i) of
              Just j  => updateEdge j i (set Hover) (updateNode i (unset Hover) g)
              Nothing => g
          else g
        Nothing => g
    E (E x y _) =>
      case angleEdge g x y of
        Just angl =>
          if minDelta a angl <= DirectionMargin || 
             minDelta a (angl + pi)  <= DirectionMargin 
            then
              let g := updateEdge x y (unset Hover) g
                  n := bestPointId a (pointAt g x, x) (pointAt g y, y)
              in updateNode n (set Hover) g
            else g
        Nothing => g
    None => g
