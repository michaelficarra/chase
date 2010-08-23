task :ruby => ['ruby:lexer','ruby:parser','ruby:test']
task :haskell => ['haskell:lexer','haskell:parser','haskell:test']

namespace 'ruby' do

	desc 'Generate lexer with rexical'
	task :lexer do
		sh 'rex *.rex --stub'
	end

	desc 'Generate parser with racc'
	task :parser do
		sh 'racc -SEv --debug -e "/usr/bin/env ruby" *.racc'
	end

	desc 'Clean up generated files and files output during debugging'
	task :clean do
		sh 'rm *.{output,rex.rb,tab.rb}'
	end

	desc 'Test the generated parser against the sample program'
	task :test do
		sh 'ruby -Ku *.tab.rb *unicode*.fol*'
	end

end


namespace 'haskell' do

	desc 'Generate lexer with alex'
	task :lexer do
		sh 'alex -g -o first-order-logic.x.hs *.x'
	end

	desc 'Generate parser with happy'
	task :parser do
		sh 'happy -i -g -c -a -d *.y'
	end

	desc 'Clean up generated files and files output during debugging'
	task :clean do
		sh 'rm a.out *.{hi,hs,info,o}'
	end

	desc 'Test the generated parser against the sample program'
	task :test do
		sh 'ghci first-order-logic.x.hs first-order-logic.hs'
	end

end
