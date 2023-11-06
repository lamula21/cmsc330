class Roster
    
    def initialize()
        @size = 0 
        @arr_person = Array.new    
    end

    def add(person)
        @arr_person.push(person)
        @size = @size + 1
    end

    def size
        return @size
    end

    def remove(person)
        if @arr_person.include?(person)
            @arr_person.delete(person)
        end
        
        @size = @size - 1
    end

    def getPerson(name)
        # Find each person in the array of Persons
        for person in @arr_person do
            if person.name == name
                return person
            end
        end

        # Person not in Roster
        return nil
    end

    def map
        if block_given?
            for each in @arr_person do
                yield each
            end
        end
    end

end

class Person
    attr_reader :name, :age

    def initialize(name, age)
        @name = name
        @age = age
    end

    def getAge
        return @age
    end

    def setAge(x)
        @age = x
    end
end

class Staff < Person
    attr_reader :position

    def initialize(name, age, position)
        super(name, age)
        @position = position
    end

    def getPosition
        return @position
    end

    def changePosition(x)
        @position = x
    end
end

class Student < Person
    attr_reader :grade

    def initialize(name, age, grade)
        super(name, age)
        @grade = grade
    end

    def getGrade
        return @grade
    end

    def changeGrade(x)
        @grade = x
    end

end