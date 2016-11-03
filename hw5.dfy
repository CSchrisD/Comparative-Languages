// Program: hw5.dfy
// Authors: Brandon Rullamas and Christina Duran
// On this homework, we worked together for 4 hours,
// Brandon worked independently for 2 hours,
// and Christina worked independently for 2 hours.
// CMPS 112
// Flanagan
// HW5

method Min(a: int, b: int) returns (c: int) 
   ensures a < b ==> c == a
   ensures b < a ==> c == b
   
{

   if (a <= b)
   {c := a;}
   else if (b < a){
     c := b;
   }
   return c; 
}

method TestMin()
{
  var m := Min(12,5);
  assert m == 5;
  var n := Min(23,42);
  assert n == 23;
}

method Search(arr: array<int>, element: int) returns (idx: int)
   requires arr != null
   requires arr.Length >= 0
   ensures idx < arr.Length ==> (arr.Length >= 0 ==> idx == -1 || idx >= 0)
   ensures arr.Length >= 0 ==> idx == -1 || (idx >= 0 ==> (idx < arr.Length && element == arr[idx]))
   ensures 0 <= idx ==> idx < arr.Length && arr[idx] == element
   ensures idx < 0 ==> forall k :: 0 <= k < arr.Length ==> arr[k] != element
   
{
  var n := 0;
  while (n < arr.Length)
    invariant 0 <= n <= arr.Length
    invariant forall k :: 0 <= k < n ==> arr[k] != element
  {
    if (arr[n] == element) {
      return n;
    }
    n := n + 1;
  }
  return -1;
}

method TestSearch()
{
  var arr := new int [3];
  arr [0] := 23;
  arr [1] := 21;
  arr [2] := 22;
  var s := Search(arr, 21);
  assert s == 1;
  var t := Search(arr, 20);
  assert t == -1;
}

method BinarySearch(arr: array<int>, element: int) returns (idx: int)
  requires arr != null && forall m,n :: 0 <= m < n < arr.Length ==> arr[m] <= arr[n]
  ensures idx >= 0 ==> idx < arr.Length && arr[idx] == element;
  ensures idx < 0 ==> forall k :: 0 <= k < arr.Length ==> arr[k] != element;
  ensures idx < arr.Length ==> (arr.Length >= 0 ==> idx == -1 || idx >= 0)
  ensures arr.Length >= 0 ==> idx == -1 || (idx >= 0 ==> (idx < arr.Length && element == arr[idx]))
{
  if (arr.Length == 0) {
    return -1;
  }
  var left := 0;
  var right := arr.Length;
  while (left < right)
    invariant 0 <= left <= right <= arr.Length;
    invariant forall j :: 0 <= j < arr.Length && !(left <= j < right) ==> arr[j] != element;
  {
    var mid := (left + right) / 2;
    if (arr[mid] == element){
      return mid;
    }
    if (arr[mid] < element) {
      left := mid + 1;
    } else {
      right := mid;
    }
  }
  return -1;
}

method TestBinarySearch()
{
  var arr := new int[3];
  arr[0] := 21;
  arr[1] := 22;
  arr[2] := 23;
  var s := BinarySearch(arr, 22);
  assert s == 1;
  var t := BinarySearch(arr, 24);
  assert t == -1;
}

