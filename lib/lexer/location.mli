type point = {
    position : int
  ; line : int
  ; column : int
}

type t = {
    source : string
  ; startAt : point
  ; endAt : point
}
