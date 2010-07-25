class RuleList
	attr_reader :length
	def initialize(*rules)
		@rules = rules
		@length = rules.length
	end
	def to_s
		"{\n" + @rules.collect{ |r| "\t"+r.to_s }.join(",\n") + "\n}"
	end
end
