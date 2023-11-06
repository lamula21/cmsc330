require "minitest/autorun"
require_relative "../../src/warmup.rb"
require_relative "../../src/roster.rb"

class PublicTests < MiniTest::Test
  def setup
  end

  def test_public_fib
    assert_equal([], fib(0))
    assert_equal([0], fib(1))
    assert_equal([0, 1], fib(2))
    assert_equal([0, 1, 1], fib(3))
    assert_equal([0, 1, 1, 2, 3, 5, 8, 13, 21, 34], fib(10))
  end

  def test_public_ispalindrome
    assert_equal(true, isPalindrome(0))
    assert_equal(true, isPalindrome(1))
    assert_equal(false, isPalindrome(10))
    assert_equal(true, isPalindrome(101))
    assert_equal(false, isPalindrome(120210))
  end

  def test_public_nthmax
    assert_equal(3, nthmax(0, [1,2,3,0]))
    assert_equal(2, nthmax(1, [3,2,1,0]))
    assert_equal(4, nthmax(2, [7,3,4,5]))
    assert_nil(nthmax(5, [1,2,3]))
  end

  def test_public_freq
    assert_equal("", freq(""))
    assert_equal("a", freq("aaabb"))
    assert_equal("a", freq("bbaaa"))
    assert_equal("s", freq("ssabcd"))
    assert_equal("x", freq("a12xxxxxyyyxyxyxy"))
  end

  def test_public_ziphash
    assert_equal({}, zipHash([], []))
    assert_equal({1 => 2}, zipHash([1], [2]))
    assert_equal({1 => 2, 5 => 4}, zipHash([1, 5], [2, 4]))
    assert_nil(zipHash([1], [2,3]))
    assert_equal({"Mamat" => "prof", "Hicks" => "prof", "Vinnie" => "TA"},
                  zipHash(["Mamat", "Hicks", "Vinnie"], ["prof", "prof", "TA"]))
  end

  def test_public_hashtoarray
    assert_equal([], hashToArray({}))
    assert_equal([["a", "b"]], hashToArray({"a" => "b"}))
    assert_equal([["a", "b"], [1, 2]], hashToArray({"a" => "b", 1 => 2}))
    assert_equal([["x", "v"], ["y", "w"], ["z", "u"]], hashToArray({"x" => "v", "y" => "w", "z" => "u"}))
  end

  def test_public_maxproc
    assert_equal(8, maxProcChain(2,[Proc.new{|x| x + 6}]))
    assert_equal(24, maxProcChain(2,[Proc.new{|x| x + 4},Proc.new{|x| x * 4}]))
    assert_equal(-1, maxProcChain(-4,[Proc.new{|x| x * 4},Proc.new{|x| x + 3}]))
  end

  def test_public_age
    age = 42
    person = Person.new("person", age)
    assert_equal(age, person.getAge)

    newAge = 84
    person.setAge(newAge)
    assert_equal(newAge, person.getAge)
  end

  def test_public_grade
    grade = 42.0
    student = Student.new("student", 0, grade)
    assert_equal(grade, student.getGrade)
    
    newGrade = 84.0
    student.changeGrade(newGrade) 
    assert_equal(newGrade, student.getGrade)
  end

  def test_public_position
    pos = "TA"
    staff = Staff.new("staff", 0, pos)
    assert_equal(pos, staff.getPosition)

    newPos = "Professor"
    staff.changePosition(newPos)
    assert_equal(newPos, staff.getPosition)
  end

  def test_public_roster
    roster = Roster.new
    assert_equal(0,roster.size)
    person = Person.new("person",21)
    roster.add(person)
    assert_equal(1,roster.size)
    roster.map{|x| x.setAge(0)}
    assert_equal(person,roster.getPerson("person"))
    assert_equal(0,roster.getPerson("person").getAge)
  end 
end
